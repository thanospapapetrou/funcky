package io.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.util.ArrayDeque;
import java.util.List;
import java.util.stream.Collectors;

import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

import io.github.thanospapapetrou.funcky.compiler.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.parser.Parser;
import io.github.thanospapapetrou.funcky.compiler.preprocessor.Preprocessor;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.TokenType;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Tokenizer;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;

public class FunckyEngine implements ScriptEngine, Compilable, Invocable {
    public static final String PARAMETER_EXTENSIONS = "io.github.thanospapapetrou.funcky.extensions";
    public static final String PARAMETER_MIME_TYPES = "io.github.thanospapapetrou.funcky.mime_types";
    public static final String PARAMETER_THREADING = "THREADING";

    private final FunckyFactory factory;
    private final FunckyContext context;
    private final Tokenizer tokenizer;
    private final Parser parser;
    private final Preprocessor preprocessor;
    private final Linker linker;
    private final FunckyJavaConverter converter;

    public FunckyList toFuncky(final List<String> list) {
        return new FunckyList(this, FunckyListType.LIST(FunckyListType.STRING).apply(this),
                list.isEmpty() ? null : new FunckyLiteral(this, toFuncky(list.getFirst())),
                list.isEmpty() ? null : new FunckyLiteral(this, toFuncky(list.subList(1, list.size()))));
    }

    public FunckyList toFuncky(final String string) {
        return new FunckyList(this, FunckyListType.STRING.apply(this),
                string.isEmpty() ? null : new FunckyLiteral(this, new FunckyCharacter(this, string.charAt(0))),
                string.isEmpty() ? null : new FunckyLiteral(this, toFuncky(string.substring(1))));
    }

    FunckyEngine(final FunckyFactory factory) {
        this.factory = factory;
        this.context = new FunckyContext();
        tokenizer = new Tokenizer(this);
        parser = new Parser(this);
        preprocessor = new Preprocessor(this);
        linker = new Linker(this);
        converter = new FunckyJavaConverter(this);
        setBindings(FunckyContext.GLOBAL.getBindings(FunckyContext.GLOBAL_SCOPE), FunckyContext.GLOBAL_SCOPE);
        setBindings(createBindings(), FunckyContext.ENGINE_SCOPE);
    }

    public Linker getLinker() {
        return linker;
    }

    public FunckyJavaConverter getConverter() {
        return converter;
    }

    @Override
    public FunckyFactory getFactory() {
        return factory;
    }

    @Override
    public FunckyContext getContext() {
        return context;
    }

    @Override
    public void setContext(final ScriptContext context) {
        for (int scope : context.getScopes()) {
            this.context.setBindings(context.getBindings(scope), scope);
        }
        this.context.setReader(context.getReader());
        this.context.setWriter(context.getWriter());
        this.context.setErrorWriter(context.getErrorWriter());
    }

    @Override
    public Bindings getBindings(final int scope) {
        return context.getBindings(FunckyContext.ENGINE_SCOPE);
    }

    @Override
    public void setBindings(final Bindings bindings, final int scope) {
        context.setBindings(bindings, scope);
    }

    @Override
    public SimpleBindings createBindings() {
        return new SimpleBindings();
    }

    @Override
    public Object get(final String key) {
        return context.getBindings(ScriptContext.ENGINE_SCOPE).get(key);
    }

    @Override
    public void put(final String key, final Object value) {
        context.getBindings(FunckyContext.ENGINE_SCOPE).put(key, value);
    }

    @Override
    public FunckyValue eval(final String expression, final ScriptContext context) throws FunckyCompilationException,
            FunckyRuntimeException {
        final FunckyExpression expr = compile(expression);
        try {
            return (expr == null) ? null : expr.eval(context);
        } catch (final SneakyRuntimeException e) {
            throw e.getCause();
        }
    }

    @Override
    public FunckyValue eval(final String expression, final Bindings bindings)
            throws FunckyCompilationException, FunckyRuntimeException {
        final Bindings engine = context.getBindings(FunckyContext.ENGINE_SCOPE);
        try {
            context.setBindings(bindings, FunckyContext.ENGINE_SCOPE);
            return eval(expression, context);
        } finally {
            context.setBindings(engine, ScriptContext.ENGINE_SCOPE);
        }
    }

    @Override
    public FunckyValue eval(final String expression) throws FunckyCompilationException, FunckyRuntimeException {
        return eval(expression, context);
    }

    @Override
    public FunckyNumber eval(final Reader script, final ScriptContext context) throws FunckyCompilationException,
            FunckyRuntimeException {
        try {
            return compile(script).eval(context);
        } catch (final SneakyRuntimeException e) {
            throw e.getCause();
        }
    }

    @Override
    public FunckyNumber eval(final Reader script, final Bindings bindings)
            throws FunckyCompilationException, FunckyRuntimeException {
        final Bindings engine = context.getBindings(FunckyContext.ENGINE_SCOPE);
        try {
            context.setBindings(bindings, FunckyContext.ENGINE_SCOPE);
            return eval(script, context);
        } finally {
            context.setBindings(engine, ScriptContext.ENGINE_SCOPE);
        }
    }

    @Override
    public FunckyNumber eval(final Reader script) throws FunckyCompilationException, FunckyRuntimeException {
        return eval(script, context);
    }

    public void compile(final URI file) throws FunckyCompilationException {
        try (final InputStreamReader input = new InputStreamReader(linker.getScript(file))) {
            compile(input, file, false);
        } catch (final IOException e) {
            throw new FunckyCompilationException(e);
        }
    }

    @Override
    public FunckyExpression compile(final String expression) throws FunckyCompilationException {
        try {
            return linker.link(preprocessor.preprocess(parser.parse(tokenizer.tokenize(expression).stream()
                    .filter(token -> token.type() != TokenType.COMMENT)
                    .collect(Collectors.toCollection(ArrayDeque::new)))));
        } catch (final SneakyCompilationException e) {
            throw e.getCause();
        }
    }

    @Override
    public FunckyScript compile(final Reader script) throws FunckyCompilationException {
        try {
            return compile(script, context.getFile(), true);
        } catch (final IOException e) {
            throw new FunckyCompilationException(e);
        }
    }

    @Override
    public Object invokeMethod(final Object object, final String method, final Object... arguments)
            throws ScriptException, NoSuchMethodException {
        // TODO
        return null;
    }

    @Override
    public Object invokeFunction(final String name, final Object... arguments)
            throws ScriptException, NoSuchMethodException {
        // TODO
        return null;
    }

    @Override
    public <T> T getInterface(final Class<T> clazz) {
        // TODO
        return null;
    }

    @Override
    public <T> T getInterface(final Object object, final Class<T> clazz) {
        // TODO
        return null;
    }

    private FunckyScript compile(final Reader script, final URI file, final boolean main)
            throws FunckyCompilationException {
        try {
            return linker.link(preprocessor.preprocess(parser.parse(tokenizer.tokenize(script, file).stream()
                    .filter(token -> token.type() != TokenType.COMMENT)
                    .collect(Collectors.toCollection(ArrayDeque::new)), file)), main);
        } catch (final SneakyCompilationException e) {
            throw e.getCause();
        }
    }
}
