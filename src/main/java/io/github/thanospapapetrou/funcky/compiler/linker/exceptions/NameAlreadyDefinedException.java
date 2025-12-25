package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;

public final class NameAlreadyDefinedException extends LinkerException {
    private static final String MESSAGE = "Name %1$s has already been defined at line %2$d";

    public NameAlreadyDefinedException(final FunckyDefinition definition, final FunckyDefinition other) {
        super(String.format(MESSAGE, definition.name(), other.line()), definition);
    }
}
