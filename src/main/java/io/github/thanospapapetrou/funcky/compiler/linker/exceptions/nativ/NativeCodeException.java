package io.github.thanospapapetrou.funcky.compiler.linker.exceptions.nativ;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.LinkerException;

public sealed class NativeCodeException extends LinkerException
        permits NativeClassNotFoundException, NativeFieldNotFoundException, NativeFieldInvalidTypeException,
        NativeConstructorNotFoundException, NativeInstantiationException {
    private static final String MESSAGE = "Error loading native code for reference `%1$s`: %2$s";

    protected NativeCodeException(final String message, final FunckyReference reference) {
        super(String.format(MESSAGE, reference, message), reference);
    }
}
