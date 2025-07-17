package io.github.thanospapapetrou.funcky.compiler.tokenizer.exceptions;

import java.net.URI;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;

public class UnrecognizedInputException extends CompilationException {
    private static final String MESSAGE = "Unrecognized input `%1$s`";

    public UnrecognizedInputException(final String input, final URI file, final int line, final int column) {
        super(String.format(MESSAGE, input), file, line, column);
    }
}
