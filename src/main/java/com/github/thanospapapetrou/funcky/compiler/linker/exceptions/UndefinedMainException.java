package com.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;

public class UndefinedMainException extends CompilationException {
    private static final String MESSAGE = "No main function defined";

    public UndefinedMainException(final FunckyScript script) {
        super(MESSAGE, script.getFile(), -1, -1);
    }
}
