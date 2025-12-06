package io.github.thanospapapetrou.funcky.compiler.preprocessor.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;

public final class InvalidArgumentIndexException extends PreprocessorException {
    private static final String MESSAGE =
            "Invalid argument index `%1$s` (should be a non-negative integer number literal))";

    public InvalidArgumentIndexException(final FunckyExpression expression) {
        super(String.format(MESSAGE, expression), expression);
    }
}
