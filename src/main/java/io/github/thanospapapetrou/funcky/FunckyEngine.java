package io.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.util.ArrayDeque;
import java.util.stream.Collectors;

import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyScriptContext;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.parser.Parser;
import io.github.thanospapapetrou.funcky.compiler.preprocessor.Preprocessor;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.TokenType;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Tokenizer;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;

public class FunckyEngine extends AbstractScriptEngine implements Compilable, Invocable {
    public static final String PARAMETER_EXTENSIONS = "io.github.thanospapapetrou.funcky.extensions";
    public static final String PARAMETER_MIME_TYPES = "io.github.thanospapapetrou.funcky.mime_types";
    public static final String PARAMETER_THREADING = "THREADING";

    private final FunckyFactory factory;
    private final Tokenizer tokenizer;
    private final Parser parser;
    private final Preprocessor preprocessor;
    private final Linker linker;
    private final FunckyJavaConverter converter;

    FunckyEngine(final FunckyFactory factory) {
        this.factory = factory;
        tokenizer = new Tokenizer();
        parser = new Parser(this);
        preprocessor = new Preprocessor();
        linker = new Linker(this);
        converter = new FunckyJavaConverter();
        setContext(FunckyScriptContext.getContext(context));
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
    public FunckyScriptContext getContext() {
        return (FunckyScriptContext) context;
    }

    @Override
    public void setContext(final ScriptContext context) {
        this.context = FunckyScriptContext.getContext(context);
    }

    @Override
    public SimpleBindings createBindings() {
        return new SimpleBindings();
    }

    @Override
    public FunckyValue eval(final String expression, final ScriptContext context)
            throws CompilationException, FunckyRuntimeException {
        final FunckyExpression expr = compile(expression);
        return (expr == null) ? null : expr.eval(context);
    }

    @Override
    public FunckyValue eval(final String expression, final Bindings bindings)
            throws CompilationException, FunckyRuntimeException {
        final FunckyExpression expr = compile(expression);
        return (expr == null) ? null : expr.eval(bindings);
    }

    @Override
    public FunckyValue eval(final String expression) throws CompilationException, FunckyRuntimeException {
        final FunckyExpression expr = compile(expression);
        return (expr == null) ? null : expr.eval();
    }

    @Override
    public FunckyNumber eval(final Reader script, final ScriptContext context)
            throws CompilationException, FunckyRuntimeException {
        return compile(script).eval(context);
    }

    @Override
    public FunckyNumber eval(final Reader script, final Bindings bindings)
            throws CompilationException, FunckyRuntimeException {
        return compile(script).eval(bindings);
    }

    @Override
    public FunckyNumber eval(final Reader script) throws CompilationException, FunckyRuntimeException {
        return compile(script).eval();
    }

    public FunckyScript compile(final URI file) throws CompilationException {
        try (final InputStreamReader input = new InputStreamReader(linker.getScript(file))) {
            return compile(input, file, false);
        } catch (final IOException e) {
            throw new CompilationException(e);
        }
    }

    @Override
    public FunckyExpression compile(final String expression) throws CompilationException {
        return linker.link(preprocessor.preprocess(parser.parse(tokenizer.tokenize(expression).stream()
                        .filter(token -> !token.getType().equals(TokenType.COMMENT))
                        .collect(Collectors.toCollection(ArrayDeque::new)))));
    }

    @Override
    public FunckyScript compile(final Reader script) throws CompilationException {
        try {
            return compile(script, getContext().getFile(), true);
        } catch (final IOException e) {
            throw new CompilationException(e);
        }
    }

    private FunckyScript compile(final Reader script, final URI file, final boolean main) throws CompilationException {
        return linker.link(preprocessor.preprocess(parser.parse(tokenizer.tokenize(script, file).stream()
                        .filter(token -> !token.getType().equals(TokenType.COMMENT))
                        .collect(Collectors.toCollection(ArrayDeque::new)), file)), main);
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
}
