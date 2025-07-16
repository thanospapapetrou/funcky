package com.github.thanospapapetrou.funcky.compiler.parser.exceptions;

import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.tokenizer.Token;

public class InvalidUriException extends CompilationException {
    private static final String MESSAGE = "Invalid URI `%1$s`";

    public InvalidUriException(final Token token) {
        super(String.format(MESSAGE, token.getStringValue()), token.getFile(), token.getLine(), token.getColumn());
    }
}
