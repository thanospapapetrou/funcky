package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;

public final class InvalidMainException extends FunckyCompilationException {
    private static final String MESSAGE = "Invalid main has type `%1$s`, it should be `%2$s`";

    public InvalidMainException(final FunckyDefinition main, final FunckyType mainType) {
        super(String.format(MESSAGE, mainType, Linker.MAIN_TYPE), main.file(), main.line(), 1);
    }
}
