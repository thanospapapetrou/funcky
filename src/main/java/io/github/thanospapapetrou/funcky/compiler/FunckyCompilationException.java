package io.github.thanospapapetrou.funcky.compiler;

import java.io.IOException;
import java.net.URI;

import javax.script.ScriptException;

import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.LinkerException;
import io.github.thanospapapetrou.funcky.compiler.parser.exceptions.ParserException;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.exceptions.TokenizerException;

public sealed class FunckyCompilationException extends ScriptException
        permits TokenizerException, ParserException, LinkerException {
    public FunckyCompilationException(final IOException e) {
        super(e);
    }

    protected FunckyCompilationException(final String message, final URI file, final int line, final int column) {
        super(message, (file == null) ? null : file.toString(), line, column);
    }
}
