package io.github.thanospapapetrou.funcky.compiler.exceptions;

public class SneakyCompilationException extends RuntimeException {
    public SneakyCompilationException(final FunckyCompilationException e) {
        super(e);
    }

    @Override
    public synchronized FunckyCompilationException getCause() {
        return (FunckyCompilationException) super.getCause();
    }
}
