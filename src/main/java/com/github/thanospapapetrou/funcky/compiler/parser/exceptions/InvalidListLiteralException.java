package com.github.thanospapapetrou.funcky.compiler.parser.exceptions;

import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import com.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public class InvalidListLiteralException extends CompilationException {
    private static final String MESSAGE =
            "Head `%1$s` with type `%2$s` can not be prepended to tail `%3$s` with type `%4$s`";

    public InvalidListLiteralException(final FunckyExpression head, final FunckyLiteral tail, final FunckyType headType,
            final FunckyListType tailType) {
        super(String.format(MESSAGE, head, headType, tail, tailType), head.getFile(), head.getLine(), head.getColumn());
    }
}
