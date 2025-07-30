package io.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayDeque;
import java.util.logging.Logger;
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
import io.github.thanospapapetrou.funcky.compiler.linker.ContextManager;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.parser.Parser;
import io.github.thanospapapetrou.funcky.compiler.preprocessor.Preprocessor;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.TokenType;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Tokenizer;
import io.github.thanospapapetrou.funcky.logging.DurationFormatter;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;

public class FunckyEngine extends AbstractScriptEngine implements Compilable, Invocable {
    public static final String PARAMETER_EXTENSIONS = "io.github.thanospapapetrou.funcky.extensions";
    public static final String PARAMETER_MIME_TYPES = "io.github.thanospapapetrou.funcky.mime_types";
    public static final String PARAMETER_THREADING = "THREADING";

    private static final Logger LOGGER = Logger.getLogger(FunckyEngine.class.getName());
    private static final String MESSAGE_COMPILED = "Compiled %1$s in %2$s";
    private static final String MESSAGE_COMPILING = "Compiling %1$s";
    private static final String MESSAGE_EVALUATED = "Evaluated %1$s in %2$s";
    private static final String MESSAGE_EVALUATING = "Evaluating %1$s";

    private final FunckyFactory factory;
    private final Tokenizer tokenizer;
    private final Parser parser;
    private final Preprocessor preprocessor;
    private final Linker linker;
    private final ContextManager manager;
    private final Clock clock;

    FunckyEngine(final FunckyFactory factory) {
        this.factory = factory;
        tokenizer = new Tokenizer();
        parser = new Parser(this);
        preprocessor = new Preprocessor();
        linker = new Linker(this);
        manager = new ContextManager(this.getContext());
        clock = Clock.systemUTC();
    }

    public ContextManager getManager() {
        return manager;
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
    public FunckyValue eval(final String expression, final ScriptContext context)
            throws CompilationException, FunckyRuntimeException {
        final FunckyExpression expr = compile(expression);
        LOGGER.info(String.format(MESSAGE_EVALUATING, Linker.STDIN));
        final Instant start = clock.instant();
        final FunckyValue value = (expr == null) ? null : expr.eval(context);
        LOGGER.info(String.format(MESSAGE_EVALUATED, Linker.STDIN,
                DurationFormatter.format(Duration.between(start, clock.instant()))));
        return value;
    }

    @Override
    public FunckyValue eval(final String expression, final Bindings bindings)
            throws CompilationException, FunckyRuntimeException {
        final FunckyExpression expr = compile(expression);
        LOGGER.info(String.format(MESSAGE_EVALUATING, Linker.STDIN));
        final Instant start = clock.instant();
        final FunckyValue value = (expr == null) ? null : expr.eval(bindings);
        LOGGER.info(String.format(MESSAGE_EVALUATED, Linker.STDIN,
                DurationFormatter.format(Duration.between(start, clock.instant()))));
        return value;
    }

    @Override
    public FunckyValue eval(final String expression) throws CompilationException, FunckyRuntimeException {
        final FunckyExpression expr = compile(expression);
        LOGGER.info(String.format(MESSAGE_EVALUATING, Linker.STDIN));
        final Instant start = clock.instant();
        final FunckyValue value = (expr == null) ? null : expr.eval();
        LOGGER.info(String.format(MESSAGE_EVALUATED, Linker.STDIN,
                DurationFormatter.format(Duration.between(start, clock.instant()))));
        return value;
    }

    @Override
    public FunckyNumber eval(final Reader script, final ScriptContext context)
            throws CompilationException, FunckyRuntimeException {
        final FunckyScript scr = compile(script);
        LOGGER.info(String.format(MESSAGE_EVALUATING, scr.getFile()));
        final Instant start = clock.instant();
        final FunckyNumber number = scr.eval(context);
        LOGGER.info(String.format(MESSAGE_EVALUATED, scr.getFile(),
                DurationFormatter.format(Duration.between(start, clock.instant()))));
        return number;
    }

    @Override
    public FunckyNumber eval(final Reader script, final Bindings bindings)
            throws CompilationException, FunckyRuntimeException {
        final FunckyScript scr = compile(script);
        LOGGER.info(String.format(MESSAGE_EVALUATING, scr.getFile()));
        final Instant start = clock.instant();
        final FunckyNumber number = scr.eval(bindings);
        LOGGER.info(String.format(MESSAGE_EVALUATED, scr.getFile(),
                DurationFormatter.format(Duration.between(start, clock.instant()))));
        return number;

    }

    @Override
    public FunckyNumber eval(final Reader script) throws CompilationException, FunckyRuntimeException {
        final FunckyScript scr = compile(script);
        LOGGER.info(String.format(MESSAGE_EVALUATING, scr.getFile()));
        final Instant start = clock.instant();
        final FunckyNumber number = scr.eval();
        LOGGER.info(String.format(MESSAGE_EVALUATED, scr.getFile(),
                DurationFormatter.format(Duration.between(start, clock.instant()))));
        return number;


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
        LOGGER.info(String.format(MESSAGE_COMPILING, Linker.STDIN));
        final Instant start = clock.instant();
        final FunckyExpression expr = linker.link(preprocessor.preprocess(parser.parse(
                tokenizer.tokenize(expression).stream().filter(token -> !token.getType().equals(TokenType.COMMENT))
                        .collect(Collectors.toCollection(ArrayDeque::new)))));
        LOGGER.info(String.format(MESSAGE_COMPILED, Linker.STDIN,
                DurationFormatter.format(Duration.between(start, clock.instant()))));
        return expr;
    }

    @Override
    public FunckyScript compile(final Reader script) throws CompilationException {
        try {
            return compile(script, getManager().getFile(), true);
        } catch (final IOException e) {
            throw new CompilationException(e);
        }
    }

    private FunckyScript compile(final Reader script, final URI file, final boolean main) throws CompilationException {
        LOGGER.info(String.format(MESSAGE_COMPILING, file));
        final Instant start = clock.instant();
        final FunckyScript scr = linker.link(preprocessor.preprocess(parser.parse(
                tokenizer.tokenize(script, file).stream().filter(token -> !token.getType().equals(TokenType.COMMENT))
                        .collect(Collectors.toCollection(ArrayDeque::new)), file)), main);
        LOGGER.info(String.format(MESSAGE_COMPILED, file,
                DurationFormatter.format(Duration.between(start, clock.instant()))));
        return scr;
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
