package io.github.thanospapapetrou.funcky.compiler.parser;

import java.net.URI;

public abstract sealed class Expression permits Literal, Reference, Application {
    protected final URI file;
    protected final int line;
    protected final int column;

    protected Expression(final URI file, final int line, final int column) {
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
}
