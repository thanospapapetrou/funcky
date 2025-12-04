package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;

public final class InvalidListLiteralException extends FunckyCompilationException {
    private static final String MESSAGE =
            "Head `%1$s` with type `%2$s` can not be prepended to tail `%3$s` with type `%4$s`";

    public InvalidListLiteralException(final FunckyEngine engine, final FunckyExpression head,
            final FunckyLiteral tail) {
        super(String.format(MESSAGE, head, (head == null) ? new FunckyTypeVariable(engine) : head.getType(), tail,
                        (tail == null) ? LIST(FunckyTypeVariable::new).apply(engine) : tail.getType()),
                (head == null) ? null : head.getFile(), (head == null) ? -1 : head.getLine(),
                (head == null) ? -1 : head.getColumn());
    }
}
