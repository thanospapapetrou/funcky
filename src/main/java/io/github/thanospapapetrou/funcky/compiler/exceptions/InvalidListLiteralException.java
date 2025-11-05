package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.LIST;

public final class InvalidListLiteralException extends FunckyCompilationException {
    private static final String MESSAGE =
            "Head `%1$s` with type `%2$s` can not be prepended to tail `%3$s` with type `%4$s`";

    public InvalidListLiteralException(final FunckyExpression head, final FunckyLiteral tail) {
        super(String.format(MESSAGE, head, (head == null) ? new FunckyTypeVariable() : head.getType(), tail,
                        (tail == null) ? LIST(new FunckyTypeVariable()) : tail.getType()),
                (head == null) ? null : head.getFile(), (head == null) ? -1 : head.getLine(),
                (head == null) ? -1 : head.getColumn());
    }
}
