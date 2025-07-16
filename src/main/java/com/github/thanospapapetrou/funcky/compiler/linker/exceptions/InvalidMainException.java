package com.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import com.github.thanospapapetrou.funcky.compiler.linker.Linker;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public class InvalidMainException extends CompilationException {
    private static final String MESSAGE = "Invalid main has type `%1$s`, it should be `%2$s`";

    public InvalidMainException(final FunckyDefinition main, final FunckyType mainType) {
        super(String.format(MESSAGE, mainType, Linker.MAIN_TYPE), main.getFile(), main.getLine(), 1);
    }
}
