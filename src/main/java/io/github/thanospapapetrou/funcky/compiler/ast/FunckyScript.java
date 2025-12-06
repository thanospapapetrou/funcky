package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.script.CompiledScript;
import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;

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
    public FunckyNumber eval(final ScriptContext context) {
            return (FunckyNumber) new FunckyApplication(new FunckyReference(engine, getFile(), -1, -1, getFile(), MAIN),
                    new FunckyLiteral(engine,
                            engine.getConverter().convert(Arrays.asList(engine.getContext().getArguments())))).eval(
                    context);
    }
}
