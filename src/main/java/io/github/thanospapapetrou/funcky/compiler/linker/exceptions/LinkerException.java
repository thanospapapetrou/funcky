package io.github.thanospapapetrou.funcky.compiler.linker.exceptions;

import io.github.thanospapapetrou.funcky.compiler.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.nativ.NativeCodeException;

public abstract sealed class LinkerException extends FunckyCompilationException
        permits PrefixAlreadyBoundException, NameAlreadyDefinedException, UnboundPrefixException, NativeCodeException,
        UndefinedNameException, InvalidListLiteralException, IllegalApplicationException, UndefinedMainException,
        InvalidMainException {
    protected LinkerException(final String message, final FunckyImport inport) {
        super(message, inport.file(), inport.line(), 1);
    }

    protected LinkerException(final String message, final FunckyDefinition definition) {
        super(message, definition.file(), definition.line(), 1);
    }

    protected LinkerException(final String message, final FunckyExpression expression) {
        super(message, (expression == null) ? null : expression.getFile(),
                (expression == null) ? -1 : expression.getLine(), (expression == null) ? -1 : expression.getColumn());
    }

    protected LinkerException(final String message, final FunckyScript script) {
        super(message, script.getFile(), 1, 1);
    }
}
