package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;

public final class UndefinedMainException extends FunckyCompilationException {
    private static final String MESSAGE = "No main function defined";

    public UndefinedMainException(final FunckyScript script) {
        super(MESSAGE, script.getFile(), -1, -1);
    }
}
