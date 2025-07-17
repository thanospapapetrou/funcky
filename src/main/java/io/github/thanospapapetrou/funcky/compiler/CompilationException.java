package io.github.thanospapapetrou.funcky.compiler;

import java.io.IOException;
import java.net.URI;

import javax.script.ScriptException;

import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;

public class CompilationException extends ScriptException {
    public CompilationException(final IOException e) {
        super(e);
    }

    public CompilationException(final FunckyRuntimeException e) {
        super(e);
    }

    protected CompilationException(final String message, final URI file, final int line, final int column) {
        super(message, file.toString(), line, column);
    }
}
