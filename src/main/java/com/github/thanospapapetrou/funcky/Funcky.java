package com.github.thanospapapetrou.funcky;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.linker.Linker;
import com.github.thanospapapetrou.funcky.compiler.parser.Parser;
import com.github.thanospapapetrou.funcky.compiler.tokenizer.Tokenizer;
import com.github.thanospapapetrou.funcky.logging.FunckyLoggerFactory;
import com.github.thanospapapetrou.funcky.runtime.FunckyValue;
import com.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;

public class Funcky {
    private static final Logger LOGGER = FunckyLoggerFactory.getLogger(Funcky.class);
    private static final String PROMPT = "%n%1$s> ";

    public static void main(final String[] arguments) {
        // TODO parse arguments better, include log level configuration via arguments?
        if (arguments.length == 0) {
            readEvalPrintLoop();
        } else {
            runScript(arguments[0], (arguments.length > 1)
                    ? Arrays.copyOfRange(arguments, 1, arguments.length) : new String[0]);
        }
    }

    private static void readEvalPrintLoop() {
        FunckyLoggerFactory.configureLogger(Tokenizer.class, Level.FINEST);
        FunckyLoggerFactory.configureLogger(Parser.class, Level.FINER);
        FunckyLoggerFactory.configureLogger(Linker.class, Level.FINE);
        FunckyLoggerFactory.configureLogger(FunckyFactory.class, Level.CONFIG);
        FunckyLoggerFactory.configureLogger(Funcky.class, Level.INFO);
        final FunckyFactory factory = new FunckyFactory();
        final FunckyEngine engine = factory.getScriptEngine();
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
            throw new RuntimeException(e); // TODO
        }
    }

    private static void runScript(final String script, final String... arguments) {
        final FunckyEngine engine = new FunckyFactory().getScriptEngine();
        engine.getContext().setFile(script);
        engine.getContext().setArguments(arguments);
        try (final InputStreamReader reader = new InputStreamReader(
                engine.getLinker().normalize(Linker.STDIN, new URI(script)).toURL().openStream())) {
            System.exit(engine.eval(reader).getValue().intValue());
        } catch (final CompilationException | FunckyRuntimeException e) {
            LOGGER.log(Level.WARNING, e.getMessage(), e);
        } catch (final URISyntaxException | IOException e) {
            throw new RuntimeException(e); // TODO
        }
    }
}
