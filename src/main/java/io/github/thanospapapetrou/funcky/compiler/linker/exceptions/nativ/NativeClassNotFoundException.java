package io.github.thanospapapetrou.funcky.compiler.linker.exceptions.nativ;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;

public final class NativeClassNotFoundException extends NativeCodeException {
    private static final String MESSAGE = "class `%1$s` not found";

    public NativeClassNotFoundException(final FunckyReference reference) {
        super(String.format(MESSAGE, reference.getNamespace().getSchemeSpecificPart()), reference);
    }
}
