package io.github.thanospapapetrou.funcky.compiler.tokenizer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.net.URI;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;
import java.util.regex.Matcher;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.exceptions.UnrecognizedInputException;
import io.github.thanospapapetrou.funcky.logging.DurationFormatter;

public class Tokenizer {
    private static final Logger LOGGER = Logger.getLogger(Tokenizer.class.getName());
    private static final String MESSAGE_TOKEN = "%1$s %2$s %3$d %4$d";
    private static final String MESSAGE_TOKENIZED = "Tokenized %1$s in %2$s";
    private static final String MESSAGE_TOKENIZING = "Tokenizing %1$s";

    private final Clock clock;

    public Tokenizer() {
        this(Clock.systemUTC());
    }

    private Tokenizer(final Clock clock) {
        this.clock = clock;
    }

    public List<Token> tokenize(final String expression) throws UnrecognizedInputException {
        LOGGER.finest(String.format(MESSAGE_TOKENIZING, Linker.STDIN));
        final Instant start = clock.instant();
        final List<Token> tokens = new ArrayList<>();
        tokenize(tokens, expression, Linker.STDIN, 1);
        addToken(tokens, TokenType.EOF, Linker.STDIN, 2, 1);
        LOGGER.finest(String.format(MESSAGE_TOKENIZED, Linker.STDIN,
                DurationFormatter.format(Duration.between(start, clock.instant()))));
        return tokens;
    }

    public List<Token> tokenize(final Reader script, final URI file) throws CompilationException {
        LOGGER.finest(String.format(MESSAGE_TOKENIZING, file));
        final Instant start = clock.instant();
        final List<Token> tokens = new ArrayList<>();
        int line = 1;
        try (final BufferedReader reader = new BufferedReader(script)) {
            String statement;
            while ((statement = reader.readLine()) != null) {
                tokenize(tokens, statement, file, line++);
            }
            addToken(tokens, TokenType.EOF, file, line, 1);
            LOGGER.finest(String.format(MESSAGE_TOKENIZED, file,
                    DurationFormatter.format(Duration.between(start, clock.instant()))));
            return tokens;
        } catch (final IOException e) {
            throw new CompilationException(e);
        }
    }

    private void tokenize(final List<Token> tokens, final String statement, final URI file, final int line)
            throws UnrecognizedInputException {
        final AtomicInteger index = new AtomicInteger(0);
        while (index.get() < statement.length()) {
            addToken(tokens, tokenize(statement, index, file, line));
        }
        addToken(tokens, TokenType.EOL, file, line, index.get() + 1);
    }

    private Token tokenize(final String statement, final AtomicInteger index, final URI file, final int line)
            throws UnrecognizedInputException {
        for (final TokenType type : TokenType.values()) {
            if (type.getPattern() != null) {
                final Matcher matcher = type.getPattern().matcher(statement.substring(index.get()));
                if (matcher.lookingAt()) {
                    final Token token = new Token(type, (matcher.groupCount() > 0) ? matcher.group() : null, file, line,
                            index.get() + 1);
                    index.set(index.get() + matcher.end());
                    return token;
                }
            }
        }
        throw new UnrecognizedInputException(statement.substring(index.get()), file, line, index.get() + 1);
    }

    private void addToken(final List<Token> tokens, final TokenType type, final URI file, final int line,
            final int column) {
        addToken(tokens, new Token(type, null, file, line, column));
    }

    private void addToken(final List<Token> tokens, final Token token) {
        tokens.add(token);
        LOGGER.finest(String.format(MESSAGE_TOKEN, token, token.getFile(), token.getLine(), token.getColumn()));
    }
}
