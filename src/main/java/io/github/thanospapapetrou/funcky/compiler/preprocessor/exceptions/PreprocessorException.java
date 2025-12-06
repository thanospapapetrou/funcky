package io.github.thanospapapetrou.funcky.compiler.preprocessor.exceptions;

import io.github.thanospapapetrou.funcky.compiler.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;

public abstract sealed class PreprocessorException extends FunckyCompilationException
        permits InvalidArgumentCountException, InvalidArgumentIndexException {
    protected PreprocessorException(final String message, final FunckyExpression expression) {
        super(message, expression.getFile(), expression.getLine(), expression.getColumn());
    }
}
