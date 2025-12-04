package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;

public final class UndefinedMainException extends LinkerException {
    private static final String MESSAGE = "No main function defined";

    public UndefinedMainException(final FunckyScript script) {
        super(MESSAGE, script);
    }
}
