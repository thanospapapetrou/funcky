package io.github.thanospapapetrou.funcky.compiler.parser.exceptions;

import io.github.thanospapapetrou.funcky.compiler.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Token;

public sealed abstract class ParserException extends FunckyCompilationException
        permits UnexpectedTokenException, InvalidUriException {
    protected ParserException(final String message, final Token token) {
        super(message, token.file(), token.line(), token.column());
    }
}
