package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;

public final class PrefixAlreadyBoundException extends LinkerException {
    private static final String MESSAGE = "Prefix %1$s has already been bound at line %2$d";

    public PrefixAlreadyBoundException(final FunckyImport inport, final FunckyImport other) {
        super(String.format(MESSAGE, inport.prefix(), other.line()), inport);
    }
}
