package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;

public final class NameAlreadyDefinedException extends FunckyCompilationException {
    private static final String MESSAGE = "Name %1$s has already been defined at line %2$d";

    public NameAlreadyDefinedException(final FunckyDefinition definition, final FunckyDefinition otherDefinition) {
        super(String.format(MESSAGE, definition.name(), otherDefinition.line()), definition.file(), definition.line(),
                1);
    }
}
