package io.github.thanospapapetrou.funcky.compiler.parser.exceptions;

import io.github.thanospapapetrou.funcky.compiler.tokenizer.Token;

public final class InvalidUriException extends ParserException {
    private static final String MESSAGE = "Invalid URI `%1$s`";

    public InvalidUriException(final Token token) {
        super(String.format(MESSAGE, token.stringValue()), token);
    }
}
