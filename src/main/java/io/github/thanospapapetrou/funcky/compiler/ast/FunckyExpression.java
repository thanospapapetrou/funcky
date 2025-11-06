package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;

import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.SimpleScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public abstract sealed class FunckyExpression extends CompiledScript
        permits FunckyLiteral, FunckyReference, FunckyApplication {
    protected final FunckyEngine engine;
    protected final URI file;
    protected final int line;
    protected final int column;
    protected final FunckyType type;

    protected FunckyExpression(final FunckyEngine engine, final URI file, final int line, final int column,
            final FunckyType type) {
        this.engine = engine;
        this.file = file;
        this.line = line;
        this.column = column;
        this.type = type;
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
        return type;
    }

    @Override
    public FunckyEngine getEngine() {
        return engine;
    }

    @Override
    public FunckyValue eval() {
        return eval((engine == null) ? new SimpleScriptContext() : engine.getContext());
    }

    @Override
    public abstract FunckyValue eval(final ScriptContext context);
}
