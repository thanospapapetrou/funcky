package com.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;

public class NameAlreadyDefinedException extends CompilationException {
    private static final String MESSAGE = "Name %1$s has already been defined at line %2$d";

    public NameAlreadyDefinedException(final FunckyDefinition definition, final FunckyDefinition otherDefinition) {
        super(String.format(MESSAGE, definition.getName(), otherDefinition.getLine()), definition.getFile(),
                definition.getLine(), 1);
    }
}
