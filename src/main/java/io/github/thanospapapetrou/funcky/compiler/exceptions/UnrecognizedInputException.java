package io.github.thanospapapetrou.funcky.compiler.exceptions;

import java.net.URI;

public final class UnrecognizedInputException extends FunckyCompilationException {
    private static final String MESSAGE = "Unrecognized input `%1$s`";

    public UnrecognizedInputException(final String input, final URI file, final int line, final int column) {
        super(String.format(MESSAGE, input), file, line, column);
    }
}
