package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;

public final class UnboundPrefixException extends FunckyCompilationException {
    private static final String MESSAGE = "Prefix `%1$s` is not bound";

    public UnboundPrefixException(final FunckyReference reference) {
        super(String.format(MESSAGE, reference.getPrefix()), reference.getFile(), reference.getLine(),
                reference.getColumn());
    }
}
