package io.github.thanospapapetrou.funcky.compiler.tokenizer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;
import java.util.regex.Matcher;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.exceptions.UnrecognizedInputException;

public class Tokenizer {
    private static final String FINEST_TOKEN = "%1$s %2$s %3$d %4$d";
    private static final Logger LOGGER = Logger.getLogger(Tokenizer.class.getName());

    public List<Token> tokenize(final String expression) throws UnrecognizedInputException {
        final List<Token> tokens = new ArrayList<>();
        tokenize(tokens, expression, Linker.STDIN, 1);
        addToken(tokens, TokenType.EOF, Linker.STDIN, 2, 1);
        LOGGER.finest("");
        return tokens;
    }

    public List<Token> tokenize(final Reader script, final URI file) throws CompilationException {
        final List<Token> tokens = new ArrayList<>();
        int line = 1;
        try (final BufferedReader reader = new BufferedReader(script)) {
            String statement;
            while ((statement = reader.readLine()) != null) {
                tokenize(tokens, statement, file, line++);
            }
            addToken(tokens, TokenType.EOF, file, line, 1);
            LOGGER.finest("");
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
        LOGGER.finest(String.format(FINEST_TOKEN, token, token.getFile(), token.getLine(), token.getColumn()));
    }
}
