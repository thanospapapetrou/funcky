package com.github.thanospapapetrou.funcky.runtime.exceptions;

public class SneakyFunckyRuntimeException extends RuntimeException {
    public SneakyFunckyRuntimeException(final FunckyRuntimeException e) {
        super(e);
    }
}
