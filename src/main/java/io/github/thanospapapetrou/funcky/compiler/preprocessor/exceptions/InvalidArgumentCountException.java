package io.github.thanospapapetrou.funcky.compiler.preprocessor.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;

public final class InvalidArgumentCountException extends PreprocessorException {
    private static final String MESSAGE =
            "Invalid argument count `%1$s` (should be a positive integer number literal))";

    public InvalidArgumentCountException(final FunckyExpression expression) {
        super(String.format(MESSAGE, expression), expression);
    }
}
