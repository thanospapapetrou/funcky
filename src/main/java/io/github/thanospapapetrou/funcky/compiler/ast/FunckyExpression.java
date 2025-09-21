package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.Map;

import javax.script.Bindings;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.SimpleScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyFactory;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public abstract sealed class FunckyExpression extends CompiledScript permits FunckyLiteral, FunckyReference, FunckyApplication {
    protected final FunckyEngine engine;
    protected final URI file;
    protected final int line;
    protected final int column;

    protected FunckyExpression(final FunckyEngine engine, final URI file, final int line, final int column) {
        this.engine = engine;
        this.file = file;
        this.line = line;
        this.column = column;
    }

    public abstract FunckyExpression normalize();

    public FunckyType getType() {
        return getType(Map.of());
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

    @Override
    public FunckyEngine getEngine() {
        return engine;
    }

    @Override
    public abstract FunckyValue eval(final ScriptContext context);

    @Override
    public FunckyValue eval(final Bindings bindings) {
        final SimpleScriptContext context = new SimpleScriptContext();
        context.setReader(engine.getContext().getReader());
        context.setWriter(engine.getContext().getWriter());
        context.setErrorWriter(engine.getContext().getErrorWriter());
        context.setBindings(engine.getContext().getBindings(ScriptContext.GLOBAL_SCOPE), ScriptContext.GLOBAL_SCOPE);
        context.setBindings((bindings == null) ? engine.getContext().getBindings(ScriptContext.ENGINE_SCOPE) : bindings,
                ScriptContext.ENGINE_SCOPE);
        return eval(context);
    }

    @Override
    public FunckyValue eval() {
        return eval((engine == null) ? FunckyFactory.GLOBAL : engine.getContext());
    }

    protected abstract FunckyType getType(final Map<FunckyReference, FunckyTypeVariable> assumptions);
}
