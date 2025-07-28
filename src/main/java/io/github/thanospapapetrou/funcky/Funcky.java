package io.github.thanospapapetrou.funcky;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Optional;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.parser.Parser;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Tokenizer;
import io.github.thanospapapetrou.funcky.logging.FunckyLogFormatter;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;

public class Funcky {
    private static final String ERROR_READING = "Error reading %1$s";
    private static final Logger LOGGER = Logger.getLogger(Funcky.class.getName());
    private static final String PROMPT = "%n%1$s> ";

    private final FunckyFactory factory;
    private final FunckyEngine engine;

    public static void main(final String[] arguments) {
        if (arguments.length == 0) {
            configureLogger(Tokenizer.class, Level.FINEST);
            configureLogger(Parser.class, Level.FINER);
            configureLogger(Linker.class, Level.FINE);
            configureLogger(FunckyEngine.class, Level.INFO);
            configureLogger(FunckyFactory.class, Level.CONFIG);
            configureLogger(Funcky.class, Level.INFO);
            new Funcky().readEvalPrintLoop();
        } else {
            new Funcky().runScript(arguments[0],
                    (arguments.length > 1) ? Arrays.copyOfRange(arguments, 1, arguments.length) : new String[0]);
        }
    }

    public static void configureLogger(final Class<?> clazz, final Level level) {
        final Logger logger = Logger.getLogger(clazz.getName());
        logger.setLevel(level);
        logger.setUseParentHandlers(false);
        final ConsoleHandler handler = new ConsoleHandler();
        handler.setLevel(level);
        handler.setFormatter(new FunckyLogFormatter());
        logger.addHandler(handler);
    }

    public Funcky() {
        this(new FunckyFactory());
    }

    private Funcky(final FunckyFactory factory) {
        this(factory, factory.getScriptEngine());
    }

    private Funcky(final FunckyFactory factory, final FunckyEngine engine) {
        this.factory = factory;
        this.engine = engine;
    }

    private void readEvalPrintLoop() {
        try (final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            String expression;
            System.out.printf(PROMPT, factory.getLanguageName());
            while ((expression = reader.readLine()) != null) {
                try {
                    Optional.ofNullable(engine.eval(expression))
                            .map(FunckyValue::toString)
                            .ifPresent(LOGGER::info);
                } catch (final CompilationException | FunckyRuntimeException e) {
                    LOGGER.log(Level.WARNING, e.getMessage(), e);
                }
                System.out.printf(PROMPT, factory.getLanguageName());
            }
        } catch (final IOException e) {
            LOGGER.log(Level.SEVERE, String.format(ERROR_READING, Linker.STDIN), e);
        }
    }

    private void runScript(final String script, final String... arguments) {
        try (final InputStreamReader reader = new InputStreamReader(Linker.normalize(Linker.STDIN,
                new URI(script)).toURL().openStream())) {
            engine.getManager().setFile(script);
            engine.getManager().setArguments(arguments);
            System.exit(engine.eval(reader).getValue().intValue());
        } catch (final CompilationException | FunckyRuntimeException e) {
            LOGGER.log(Level.WARNING, e.getMessage(), e);
        } catch (final URISyntaxException | IOException e) {
            LOGGER.log(Level.SEVERE, String.format(ERROR_READING, script), e);
        }
    }
}
