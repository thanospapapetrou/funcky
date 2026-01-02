package io.github.thanospapapetrou.funcky.compiler.linker.exceptions.nativ;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;

public final class NativeConstructorNotFoundException extends NativeCodeException {
    private static final String MESSAGE = "class `%1$s` must declare a public constructor with a single argument of type `%2$s`";

    public NativeConstructorNotFoundException(final FunckyReference reference) {
        super(String.format(MESSAGE, reference.getNamespace().getSchemeSpecificPart(), FunckyContext.class.getName()), reference);
    }
}
