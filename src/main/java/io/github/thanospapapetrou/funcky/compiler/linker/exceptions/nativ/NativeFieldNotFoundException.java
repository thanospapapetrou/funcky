package io.github.thanospapapetrou.funcky.compiler.linker.exceptions.nativ;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;

public final class NativeFieldNotFoundException extends NativeCodeException {
    private static final String MESSAGE = "class `%1$s` contains no field `%2$s`";

    public NativeFieldNotFoundException(final FunckyReference reference) {
        super(String.format(MESSAGE, reference.getNamespace().getSchemeSpecificPart(), reference.getName()), reference);
    }
}
