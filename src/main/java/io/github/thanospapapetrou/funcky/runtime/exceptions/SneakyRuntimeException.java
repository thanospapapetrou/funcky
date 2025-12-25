package io.github.thanospapapetrou.funcky.runtime.exceptions;

import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;

public class SneakyRuntimeException extends RuntimeException {
    public SneakyRuntimeException(final String message, final FunckyContext context) {
        this(new FunckyRuntimeException(message, context));
    }

    public SneakyRuntimeException(final FunckyRuntimeException e) {
        super(e);
    }

    @Override
    public synchronized FunckyRuntimeException getCause() {
        return (FunckyRuntimeException) super.getCause();
    }
}
