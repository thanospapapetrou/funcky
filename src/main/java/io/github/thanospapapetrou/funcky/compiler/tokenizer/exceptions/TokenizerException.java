package io.github.thanospapapetrou.funcky.compiler.tokenizer.exceptions;

import java.net.URI;

import io.github.thanospapapetrou.funcky.compiler.FunckyCompilationException;

public abstract sealed class TokenizerException extends FunckyCompilationException
        permits UnrecognizedInputException {
    protected TokenizerException(final String message, final URI file, final int line, final int column) {
        super(message, file, line, column);
    }
}
