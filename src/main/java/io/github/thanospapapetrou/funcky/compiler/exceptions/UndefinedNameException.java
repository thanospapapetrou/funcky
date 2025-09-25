package io.github.thanospapapetrou.funcky.compiler.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;

public final class UndefinedNameException extends FunckyCompilationException {
    private static final String MESSAGE = "Name `%1$s` is not defined in namespace `%2$s`";

    public UndefinedNameException(final FunckyReference reference) {
        super(String.format(MESSAGE, reference.getName(), reference.getNamespace()), reference.getFile(),
                reference.getLine(), reference.getColumn());
    }
}
