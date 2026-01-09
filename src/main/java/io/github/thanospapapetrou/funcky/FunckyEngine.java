package io.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayDeque;
import java.util.List;
import java.util.logging.Logger;
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
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
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

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING;

public class FunckyEngine implements ScriptEngine, Compilable, Invocable {
    public static final String PARAMETER_EXTENSIONS = "io.github.thanospapapetrou.funcky.extensions";
    public static final String PARAMETER_MIME_TYPES = "io.github.thanospapapetrou.funcky.mime_types";
    public static final String PARAMETER_THREADING = "THREADING";
    private static final Logger LOGGER = Logger.getLogger(FunckyEngine.class.getName());
    private static final String MESSAGE_COMPILATION = "Compilation finished in %1$d ms";
    private static final String MESSAGE_EVALUATION = "Evaluation finished in %1$d ms";

    private final FunckyFactory factory;
    private final FunckyContext context;
    private final Tokenizer tokenizer;
    private final Parser parser;
    private final Preprocessor preprocessor;
    private final Linker linker;
    private final Clock clock;

    public FunckyList toFuncky(final List<String> list) {
        return new FunckyList(context, LIST(STRING).apply(context),
                list.isEmpty() ? null : new FunckyLiteral(toFuncky(list.getFirst())),
                list.isEmpty() ? null : new FunckyLiteral(toFuncky(list.subList(1, list.size()))));
    }

    public FunckyList toFuncky(final String string) {
        return new FunckyList(context, STRING.apply(context),
                string.isEmpty() ? null : new FunckyLiteral(new FunckyCharacter(context, string.charAt(0))),
                string.isEmpty() ? null : new FunckyLiteral(toFuncky(string.substring(1))));
    }

    FunckyEngine(final FunckyFactory factory) {
        this(factory, Clock.systemUTC());
    }

    private FunckyEngine(final FunckyFactory factory, final Clock clock) {
        this.factory = factory;
        context = new FunckyContext();
        tokenizer = new Tokenizer(clock);
        parser = new Parser(this, clock);
        preprocessor = new Preprocessor(this);
        linker = new Linker(this, clock);
        this.clock = clock;
        setBindings(createBindings(), FunckyContext.GLOBAL_SCOPE);
        setBindings(createBindings(), FunckyContext.ENGINE_SCOPE);
        setBindings(createBindings(), FunckyContext.SCRIPTS_SCOPE);
        context.setEngine(this);
    }

    public Linker getLinker() {
        return linker;
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
        return context.getBindings(scope);
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
        final FunckyScript script = compile(expression);
        if (script == null) {
            return null;
        }
        try {
            final Instant start = clock.instant();
            final FunckyValue result = script.getDefinitions().stream()
                    .filter(definition -> definition.name().equals(FunckyScript.IT))
                    .map(FunckyDefinition::expression)
                    .findFirst()
                    .get()
                    .eval(FunckyContext.toFuncky(context)); // TODO eval script via main
            LOGGER.info(String.format(MESSAGE_EVALUATION, Duration.between(start, clock.instant()).toMillis()));
            return result;
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
            final Instant start = clock.instant();
            final FunckyNumber result = compile(script).eval(context);
            LOGGER.info(String.format(MESSAGE_EVALUATION, Duration.between(start, clock.instant()).toMillis()));
            return result;
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
        try (final InputStreamReader input = new InputStreamReader(Linker.getScript(file).openStream())) {
            compile(input, file, false);
        } catch (final IOException e) {
            throw new FunckyCompilationException(e);
        }
    }

    @Override
    public FunckyScript compile(final String expression) throws FunckyCompilationException {
        try {
            final Instant start = clock.instant();
            final FunckyScript script = linker.link(preprocessor.preprocess(parser.parse(
                    tokenizer.tokenize(expression).stream()
                            .filter(token -> token.type() != TokenType.COMMENT)
                            .collect(Collectors.toCollection(ArrayDeque::new)))));
            LOGGER.info(String.format(MESSAGE_COMPILATION, Duration.between(start, clock.instant()).toMillis()));
            return script;
        } catch (final SneakyCompilationException e) {
            throw e.getCause();
        }
    }

    @Override
    public FunckyScript compile(final Reader script) throws FunckyCompilationException {
        try {
            final Instant start = clock.instant();
            final FunckyScript compiled = compile(script, context.getFile(), true);
            LOGGER.info(String.format(MESSAGE_COMPILATION, Duration.between(start, clock.instant()).toMillis()));
            return compiled;
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
