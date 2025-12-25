package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;

public final class InvalidListLiteralException extends LinkerException {
    private static final String MESSAGE =
            "Head `%1$s` with type `%2$s` can not be prepended to tail `%3$s` with type `%4$s`";

    public InvalidListLiteralException(final FunckyEngine engine, final FunckyExpression head,
            final FunckyExpression tail) { // TODO remove engine
        super(String.format(MESSAGE, head.toString(engine.getContext()),
                        (head == null) ? new FunckyTypeVariable(engine) : head.getType(engine.getContext()),
                        tail.toString(engine.getContext()),
                        (tail == null) ? LIST(FunckyTypeVariable::new).apply(engine) : tail.getType(engine.getContext())),
                head);
    }
}
