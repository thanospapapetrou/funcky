package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;

public final class InvalidMainException extends FunckyCompilationException {
    private static final String MESSAGE = "Invalid main has type `%1$s`, it should be `%2$s`";

    public InvalidMainException(final FunckyDefinition main) {
        super(String.format(MESSAGE, main.expression().getType(), Linker.MAIN_TYPE), main.file(), main.line(), 1);
    }
}
