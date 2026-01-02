package io.github.thanospapapetrou.funcky.compiler.linker.exceptions.nativ;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public final class NativeFieldInvalidTypeException extends NativeCodeException {
    private static final String MESSAGE = "field `%1$s` of class `%2$s` must have type `%3$s`";

    public NativeFieldInvalidTypeException(final FunckyReference reference) {
        super(String.format(MESSAGE, reference.getName(), reference.getNamespace().getSchemeSpecificPart(),
                FunckyValue.class.getName()), reference);
    }
}
