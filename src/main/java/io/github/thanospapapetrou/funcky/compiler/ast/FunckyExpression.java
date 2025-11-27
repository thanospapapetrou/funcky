package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.Map;

import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public abstract sealed class FunckyExpression permits FunckyLiteral, FunckyReference, FunckyApplication {
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

    public FunckyType getType() {
        return getType(Map.of());
    }

    public abstract FunckyExpression normalize();

    public abstract FunckyValue eval();

    protected abstract FunckyType getType(final Map<FunckyReference, FunckyTypeVariable> assumptions);
}
