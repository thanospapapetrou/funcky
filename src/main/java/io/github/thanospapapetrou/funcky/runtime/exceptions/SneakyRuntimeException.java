package io.github.thanospapapetrou.funcky.runtime.exceptions;

public class SneakyRuntimeException extends RuntimeException {
    public SneakyRuntimeException(final String message) {
        this(new FunckyRuntimeException(message));
    }

    public SneakyRuntimeException(final FunckyRuntimeException e) {
        super(e);
    }

    @Override
    public synchronized FunckyRuntimeException getCause() {
        return (FunckyRuntimeException) super.getCause();
    }
}
