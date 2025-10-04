package io.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URI;
import java.util.ArrayDeque;
import java.util.stream.Collectors;

import javax.script.AbstractScriptEngine;
import javax.script.Compilable;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.exceptions.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.linker.ContextManager;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.parser.Parser;
import io.github.thanospapapetrou.funcky.compiler.preprocessor.Preprocessor;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.TokenType;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Tokenizer;
import io.github.thanospapapetrou.funcky.compiler.transpiler.Transpiler;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public class FunckyEngine extends AbstractScriptEngine implements Compilable, Invocable {
    public static final String PARAMETER_EXTENSIONS = "io.github.thanospapapetrou.funcky.extensions";
    public static final String PARAMETER_MIME_TYPES = "io.github.thanospapapetrou.funcky.mime_types";
    public static final String PARAMETER_THREADING = "THREADING";

    private final FunckyFactory factory;
    private final Tokenizer tokenizer;
    private final Parser parser;
    private final Preprocessor preprocessor;
    private final Linker linker;
    private final ContextManager manager;
    private final Transpiler transpiler;
    private final FunckyJavaConverter converter;

    FunckyEngine(final FunckyFactory factory) {
        this.factory = factory;
        tokenizer = new Tokenizer(this);
        parser = new Parser(this);
        preprocessor = new Preprocessor(this);
        linker = new Linker(this);
        manager = new ContextManager(this.getContext());
        try {
            transpiler = new Transpiler(this);
        } catch (final MalformedURLException e) {
            throw new RuntimeException(e); // TODO
        }
        converter = new FunckyJavaConverter(this);
    }

    public Linker getLinker() {
        return linker;
    }

    public ContextManager getManager() {
        return manager;
    }

    public Transpiler getTranspiler() {
        return transpiler;
    }

    public FunckyJavaConverter getConverter() {
        return converter;
    }

    @Override
    public FunckyFactory getFactory() {
        return factory;
    }

    @Override
    public SimpleBindings createBindings() {
        return new SimpleBindings();
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
    public FunckyNumber eval(final Reader script, final ScriptContext context) throws FunckyCompilationException,
            FunckyRuntimeException {
        try {
            return compile(script).eval(context);
        } catch (final SneakyRuntimeException e) {
            throw e.getCause();
        }
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
            return transpiler.transpile(
                    linker.link(preprocessor.preprocess(parser.parse(tokenizer.tokenize(expression).stream()
                    .filter(token -> !token.type().equals(TokenType.COMMENT))
                            .collect(Collectors.toCollection(ArrayDeque::new))))));
        } catch (final SneakyCompilationException e) {
            throw e.getCause();
        }
    }

    @Override
    public FunckyScript compile(final Reader script) throws FunckyCompilationException {
        try {
            return compile(script, getManager().getFile(), true);
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
            return transpiler.transpile(linker.link(preprocessor.preprocess(parser.parse(
                    tokenizer.tokenize(script, file).stream().filter(token -> !token.type().equals(TokenType.COMMENT))
                            .collect(Collectors.toCollection(ArrayDeque::new)), file)), main));
        } catch (final SneakyCompilationException e) {
            throw e.getCause();
        }
    }
}
