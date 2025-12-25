package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public final class InvalidMainException extends LinkerException {
    private static final String MESSAGE = "Invalid main has type `%1$s`, it should be `%2$s`";

    // TODO remove engine
    public InvalidMainException(final FunckyDefinition main, final FunckyType type, final FunckyEngine engine) {
        super(String.format(MESSAGE, type,
                Linker.MAIN_TYPE.apply(engine)), main);
    }
}
