package io.github.thanospapapetrou.funcky.compiler.linker.exceptions.nativ;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;

public final class NativeInstantiationException extends NativeCodeException {
    private static final String MESSAGE = "errorr instantiating class `%1$s: %2$s`";

    public NativeInstantiationException(final FunckyReference reference, final String message) {
        super(String.format(MESSAGE, reference.getNamespace().getSchemeSpecificPart(), message), reference);
    }
}
