package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public final class IllegalApplicationException extends LinkerException {
    private static final String MESSAGE =
            "Function `%1$s` with type `%2$s` can not be applied to argument `%3$s` with type `%4$s`";

    public IllegalApplicationException(final FunckyApplication application, final FunckyType functionType,
            final FunckyType argumentType) {
        super(String.format(MESSAGE, application.getFunction(), functionType, application.getArgument(), argumentType),
                application);
    }
}
