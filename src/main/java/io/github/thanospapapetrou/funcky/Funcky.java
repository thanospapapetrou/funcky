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

import io.github.thanospapapetrou.funcky.compiler.exceptions.FunckyCompilationException;
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

    private final FunckyEngine engine;

    public static void main(final String[] arguments) throws IOException {
        if (arguments.length == 0) {
            configureLogger(Tokenizer.class, Level.FINEST);
            configureLogger(Parser.class, Level.FINER);
            configureLogger(Linker.class, Level.FINE);
            configureLogger(FunckyFactory.class, Level.CONFIG);
            configureLogger(Funcky.class, Level.INFO);
            new Funcky().readEvalPrintLoop();
        } else {
            new Funcky().runScript(arguments[0], Arrays.copyOfRange(arguments, 1, arguments.length));
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

    public Funcky() throws IOException {
        this(new FunckyFactory());
    }

    private Funcky(final FunckyFactory factory) {
        this(factory.getScriptEngine());
    }

    private Funcky(final FunckyEngine engine) {
        this.engine = engine;
    }

    private void readEvalPrintLoop() {
        try (final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            String expression;
            System.out.printf(PROMPT, engine.getFactory().getLanguageName());
            while ((expression = reader.readLine()) != null) {
                try {
                    Optional.ofNullable(engine.eval(expression, engine.getContext()))
                            .map(FunckyValue::toString)
                            .ifPresent(LOGGER::info);
                } catch (final FunckyCompilationException | FunckyRuntimeException e) {
                    LOGGER.log(Level.WARNING, e.getMessage(), e);
                }
                System.out.printf(PROMPT, engine.getFactory().getLanguageName());
            }
        } catch (final IOException e) {
            LOGGER.log(Level.SEVERE, String.format(ERROR_READING, engine.getLinker().getStdin()), e);
        }
    }

    private void runScript(final String script, final String... arguments) {
        try (final InputStreamReader reader = new InputStreamReader(engine.getLinker().normalize(engine.getLinker()
                .getStdin(), new URI(script)).toURL().openStream())) {
            engine.getManager().setFile(script);
            engine.getManager().setArguments(arguments);
            System.exit(engine.eval(reader, engine.getContext()).getValue().intValue());
        } catch (final FunckyCompilationException | FunckyRuntimeException e) {
            LOGGER.log(Level.WARNING, e.getMessage(), e);
        } catch (final IOException | URISyntaxException e) {
            LOGGER.log(Level.SEVERE, String.format(ERROR_READING, script), e);
        }
    }
}
