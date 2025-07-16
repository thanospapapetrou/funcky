package com.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;

public class PrefixAlreadyBoundException extends CompilationException {
    private static final String MESSAGE = "Prefix %1$s has already been bound at line %2$d";

    public PrefixAlreadyBoundException(final FunckyImport inport, final FunckyImport otherImport) {
        super(String.format(MESSAGE, inport.getPrefix(), otherImport.getLine()), inport.getFile(), inport.getLine(), 1);
    }
}
