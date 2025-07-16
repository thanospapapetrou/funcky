package com.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.Map;

import javax.script.Bindings;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.SimpleScriptContext;

import com.github.thanospapapetrou.funcky.FunckyEngine;
import com.github.thanospapapetrou.funcky.FunckyFactory;
import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.linker.exceptions.UnboundPrefixException;
import com.github.thanospapapetrou.funcky.runtime.FunckyValue;
import com.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public abstract class FunckyExpression extends CompiledScript {
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

    public abstract FunckyExpression normalize() throws UnboundPrefixException;

    public FunckyType getType() throws CompilationException {
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
    public abstract FunckyValue eval(final ScriptContext context) throws FunckyRuntimeException;

    @Override
    public FunckyValue eval(final Bindings bindings) throws FunckyRuntimeException {
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
    public FunckyValue eval() throws FunckyRuntimeException {
        return eval(((engine == null) ? FunckyFactory.ENGINE : engine).getContext());
    }

    protected abstract FunckyType getType(final Map<FunckyReference, FunckyTypeVariable> assumptions)
            throws CompilationException;
}
