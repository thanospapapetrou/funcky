package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;

public record FunckyImport(FunckyEngine engine, URI file, int line, String prefix, URI namespace) {
    private static final String FORMAT = "%1$s: \"%2$s\"";

    public FunckyImport canonicalize() {
        return new FunckyImport(engine, file, line, prefix, engine.getLinker().canonicalize(file, namespace));
    }

    @Override
    public String toString() {
        return String.format(FORMAT, prefix, EscapeHelper.escape(namespace.toString()));
    }
}
