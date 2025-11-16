package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.Definition;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyDefinition;

public final class NameAlreadyDefinedException extends FunckyCompilationException {
    private static final String MESSAGE = "Name %1$s has already been defined at line %2$d";

    public NameAlreadyDefinedException(final Definition definition, final Definition other) {
        super(String.format(MESSAGE, definition.name(), other.line()), definition.file(), definition.line(),
                1);
    }
}
