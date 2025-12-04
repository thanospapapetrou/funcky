package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;

public final class UnboundPrefixException extends LinkerException {
    private static final String MESSAGE = "Prefix `%1$s` is not bound";

    public UnboundPrefixException(final FunckyReference reference) {
        super(String.format(MESSAGE, reference.getPrefix()), reference);
    }
}
