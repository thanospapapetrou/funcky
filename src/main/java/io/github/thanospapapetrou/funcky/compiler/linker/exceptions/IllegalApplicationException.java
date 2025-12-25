package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;

public final class IllegalApplicationException extends LinkerException {
    private static final String MESSAGE =
            "Function `%1$s` with type `%2$s` can not be applied to argument `%3$s` with type `%4$s`";

    public IllegalApplicationException(final FunckyApplication application) {
        super(String.format(MESSAGE, application.getFunction(), application.getFunction().getType(),
                application.getArgument(), application.getArgument().getType()), application);
    }
}
