package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public final class InvalidListLiteralException extends LinkerException {
    private static final String MESSAGE =
            "Head `%1$s` with type `%2$s` can not be prepended to tail `%3$s` with type `%4$s`";

    public InvalidListLiteralException(final FunckyExpression head, final FunckyType headType,
            final FunckyExpression tail, final FunckyListType tailType) {
        super(String.format(MESSAGE, head, headType, tail, tailType), head);
    }
}
