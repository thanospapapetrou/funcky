package io.github.thanospapapetrou.funcky.compiler.parser.exceptions;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public class InvalidListLiteralException extends CompilationException {
    private static final String MESSAGE =
            "Head `%1$s` with type `%2$s` can not be prepended to tail `%3$s` with type `%4$s`";

    public InvalidListLiteralException(final FunckyExpression head, final FunckyLiteral tail, final FunckyType headType,
            final FunckyListType tailType) {
        super(String.format(MESSAGE, head, headType, tail, tailType), head.getFile(), head.getLine(), head.getColumn());
    }
}
