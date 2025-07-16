package com.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.script.Bindings;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.SimpleScriptContext;

import com.github.thanospapapetrou.funcky.FunckyEngine;
import com.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import com.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import com.github.thanospapapetrou.funcky.runtime.exceptions.SneakyFunckyRuntimeException;

public class FunckyScript extends CompiledScript {
    public static final String MAIN = "main";

    protected final FunckyEngine engine;
    protected final URI file;
    protected final List<FunckyImport> imports;
    protected final List<FunckyDefinition> definitions;

    public FunckyScript(final FunckyEngine engine, final URI file) {
        this(engine, file, new ArrayList<>(), new ArrayList<>());
    }

    private FunckyScript(final FunckyEngine engine, final URI file, final List<FunckyImport> imports,
            final List<FunckyDefinition> definitions) {
        this.engine = engine;
        this.file = file;
        this.imports = imports;
        this.definitions = definitions;
    }

    public URI getFile() {
        return file;
    }

    public List<FunckyImport> getImports() {
        return imports;
    }

    public List<FunckyDefinition> getDefinitions() {
        return definitions;
    }

    @Override
    public FunckyEngine getEngine() {
        return engine;
    }

    @Override
    public FunckyNumber eval(final ScriptContext context) throws FunckyRuntimeException {
        try {
            return (FunckyNumber) new FunckyApplication(new FunckyReference(engine, getFile(), -1, -1, getFile(), MAIN),
                    new FunckyLiteral(engine,
                            engine.getConverter().convert(Arrays.asList(engine.getContext().getArguments())))).eval(
                    context);
        } catch (final SneakyFunckyRuntimeException e) {
            throw (FunckyRuntimeException) e.getCause();
        }
    }

    @Override
    public FunckyNumber eval(final Bindings bindings) throws FunckyRuntimeException {
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
    public FunckyNumber eval() throws FunckyRuntimeException {
        return eval(engine.getContext());
    }
}
