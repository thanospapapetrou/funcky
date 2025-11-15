package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.Import;

public final class PrefixAlreadyBoundException extends FunckyCompilationException {
    private static final String MESSAGE = "Prefix %1$s has already been bound at line %2$d";

    public PrefixAlreadyBoundException(final Import inport, final Import otherImport) {
        super(String.format(MESSAGE, inport.prefix(), otherImport.line()), inport.file(), inport.line(), 1);
    }
}
