package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;

public final class InvalidListLiteralException extends FunckyCompilationException {
    private static final String MESSAGE =
            "Head `%1$s` with type `%2$s` can not be prepended to tail `%3$s` with type `%4$s`";

    public InvalidListLiteralException(final FunckyExpression head, final FunckyLiteral tail) {
        super(String.format(MESSAGE, head, (head == null) ? new FunckyTypeVariable() : head.getType(), tail,
                (tail == null) ? new FunckyListType(new FunckyTypeVariable()) : tail.getType()), head.getFile(),
                head.getLine(), head.getColumn());
    }
}
