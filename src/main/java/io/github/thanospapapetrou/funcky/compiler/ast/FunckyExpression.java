package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;

import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public abstract sealed class FunckyExpression
        permits FunckyLiteral, FunckyReference, FunckyApplication {
    protected final URI file;
    protected final int line;
    protected final int column;

    protected FunckyExpression(final URI file, final int line, final int column) {
        this.file = file;
        this.line = line;
        this.column = column;
    }

    public URI getFile() {
        return file;
    }

    public int getLine() {
        return line;
    }

    public int getColumn() {
        return column;
    }

    public abstract FunckyType getType(final FunckyContext context);

    public abstract FunckyValue eval(final FunckyContext context);

    public abstract String toString(final boolean canonical, final FunckyContext context);

    public String toString(final FunckyContext context) {
        return toString(false, context);
    }
}
