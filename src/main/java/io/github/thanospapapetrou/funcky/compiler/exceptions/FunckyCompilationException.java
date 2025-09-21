package io.github.thanospapapetrou.funcky.compiler.exceptions;

import java.io.IOException;
import java.net.URI;

import javax.script.ScriptException;

public sealed class FunckyCompilationException extends ScriptException
        permits UnrecognizedInputException, UnexpectedTokenException, InvalidUriException, InvalidListLiteralException,
        PrefixAlreadyBoundException, NameAlreadyDefinedException, UnboundPrefixException, UndefinedNameException,
        IllegalApplicationException, UndefinedMainException, InvalidMainException {
    public FunckyCompilationException(final IOException e) {
        super(e);
    }

    protected FunckyCompilationException(final String message, final URI file, final int line, final int column) {
        super(message, file.toString(), line, column);
    }
}
