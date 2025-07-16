package com.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;

public class UnboundPrefixException extends CompilationException {
    private static final String MESSAGE = "Prefix `%1$s` is not bound";

    public UnboundPrefixException(final FunckyReference reference) {
        super(String.format(MESSAGE, reference.getPrefix()), reference.getFile(), reference.getLine(),
                reference.getColumn());
    }
}
