package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;

public class PrefixAlreadyBoundException extends CompilationException {
    private static final String MESSAGE = "Prefix %1$s has already been bound at line %2$d";

    public PrefixAlreadyBoundException(final FunckyImport inport, final FunckyImport otherImport) {
        super(String.format(MESSAGE, inport.prefix(), otherImport.line()), inport.file(), inport.line(), 1);
    }
}
