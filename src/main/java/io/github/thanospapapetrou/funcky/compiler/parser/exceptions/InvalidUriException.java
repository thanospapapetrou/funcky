package io.github.thanospapapetrou.funcky.compiler.parser.exceptions;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Token;

public class InvalidUriException extends CompilationException {
    private static final String MESSAGE = "Invalid URI `%1$s`";

    public InvalidUriException(final Token token) {
        super(String.format(MESSAGE, token.getStringValue()), token.getFile(), token.getLine(), token.getColumn());
    }
}
