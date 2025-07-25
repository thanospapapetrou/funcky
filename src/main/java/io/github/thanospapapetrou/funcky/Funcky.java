package io.github.thanospapapetrou.funcky;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.parser.Parser;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Tokenizer;
import io.github.thanospapapetrou.funcky.logging.FunckyLogFormatter;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;

public class Funcky {
    private static final String DURATION_DELIMITER = " ";
    private static final String DURATION_HOURS = "%1$d h";
    private static final String DURATION_MILLIS = "%1$d ms";
    private static final String DURATION_MINUTES = "%1$d m";
    private static final String DURATION_PREFIX = "(";
    private static final String DURATION_SECONDS = "%1$d s";
    private static final String DURATION_SUFFIX = ")";
    private static final Logger LOGGER = Logger.getLogger(Funcky.class.getName());
    private static final String PROMPT = "%n%1$s> ";

    private final FunckyFactory factory;
    private final FunckyEngine engine;
    private final Clock clock;

    public static void main(final String[] arguments) {
        if (arguments.length == 0) {
            configureLogger(Tokenizer.class, Level.FINEST);
            configureLogger(Parser.class, Level.FINER);
            configureLogger(Linker.class, Level.FINE);
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
        this(factory, factory.getScriptEngine(), Clock.systemUTC());
    }

    private Funcky(final FunckyFactory factory, final FunckyEngine engine, final Clock clock) {
        this.factory = factory;
        this.engine = engine;
        this.clock = clock;
    }

    private void readEvalPrintLoop() {
        try (final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            String expression;
            System.out.printf(PROMPT, factory.getLanguageName());
            while ((expression = reader.readLine()) != null) {
                try {
                    final Instant start = clock.instant();
                    Optional.ofNullable(engine.eval(expression))
                            .map(FunckyValue::toString)
                            .ifPresent(LOGGER::info);
                    LOGGER.info(formatDuration(Duration.between(start, clock.instant())));
                } catch (final CompilationException | FunckyRuntimeException e) {
                    LOGGER.log(Level.WARNING, e.getMessage(), e);
                }
                System.out.printf(PROMPT, factory.getLanguageName());
            }
        } catch (final IOException e) {
            throw new RuntimeException(e); // TODO
        }
    }

    private void runScript(final String script, final String... arguments) {
        try (final InputStreamReader reader = new InputStreamReader(Linker.normalize(Linker.STDIN,
                new URI(script)).toURL().openStream())) {
            System.exit(engine.eval(reader).getValue().intValue());
        } catch (final CompilationException | FunckyRuntimeException e) {
            LOGGER.log(Level.WARNING, e.getMessage(), e);
        } catch (final URISyntaxException | IOException e) {
            throw new RuntimeException(e); // TODO
        }
    }

    private String formatDuration(final Duration duration) {
        final long hours = duration.toHours();
        final long minutes = duration.minus(Duration.ofHours(hours)).toMinutes();
        final long seconds = duration.minus(Duration.ofHours(hours)).minus(Duration.ofMinutes(minutes)).toSeconds();
        final long millis = duration.minus(Duration.ofHours(hours)).minus(Duration.ofMinutes(minutes))
                .minus(Duration.ofSeconds(seconds)).toMillis();
        return DURATION_PREFIX + Map.of(
                        DURATION_HOURS, hours,
                        DURATION_MINUTES, minutes,
                        DURATION_SECONDS, seconds,
                        DURATION_MILLIS, millis
                ).entrySet().stream()
                .filter(d -> d.getValue() > 0)
                .map(d -> String.format(d.getKey(), d.getValue()))
                .collect(Collectors.joining(DURATION_DELIMITER)) + DURATION_SUFFIX;
    }
}
