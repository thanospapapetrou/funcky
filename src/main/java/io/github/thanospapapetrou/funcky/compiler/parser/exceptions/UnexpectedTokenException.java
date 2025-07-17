package io.github.thanospapapetrou.funcky.compiler.parser.exceptions;

import java.util.SortedSet;
import java.util.stream.Collectors;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Token;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.TokenType;

public class UnexpectedTokenException extends CompilationException {
    private static final String DELIMITER = ", ";
    private static final String MESSAGE = "Unexpected token %1$s, expected %2$s";

    public UnexpectedTokenException(final Token token, final SortedSet<TokenType> expected) {
        super(String.format(MESSAGE, token, expected.stream()
                        .map(TokenType::toString)
                        .collect(Collectors.joining(DELIMITER))),
                token.getFile(), token.getLine(), token.getColumn());
    }
}
