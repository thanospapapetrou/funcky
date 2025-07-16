package com.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import java.net.URI;

import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;

public class UndefinedNameException extends CompilationException {
    private static final String MESSAGE = "Name `%1$s` is not defined in namespace `%2$s`";

    public UndefinedNameException(final URI namespace, final FunckyReference reference) {
        super(String.format(MESSAGE, reference.getName(), namespace), reference.getFile(), reference.getLine(),
                reference.getColumn());
    }
}
