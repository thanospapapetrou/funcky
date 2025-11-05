package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.SimpleScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyJavaConverter;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;

public class FunckyScript extends CompiledScript {
    public static final String IT = "it";
    public static final String MAIN = "main";

    protected final FunckyEngine engine;
    protected final URI file;
    protected final List<FunckyImport> imports;
    protected final List<FunckyDefinition> definitions;

    public FunckyScript(final FunckyEngine engine, final URI file) {
        this(engine, file, new ArrayList<>(), new ArrayList<>());
    }

    public FunckyScript(final FunckyExpression expression) {
        this(expression.getEngine(), Linker.STDIN);
        definitions.add(new FunckyDefinition(Linker.STDIN, 1, FunckyScript.IT, expression));
    }

    private FunckyScript(final FunckyEngine engine, final URI file, final List<FunckyImport> imports,
            final List<FunckyDefinition> definitions) {
        this.engine = engine;
        this.file = file;
        this.imports = imports;
        this.definitions = definitions;
    }

    public URI getFile() {
        return (file == null) ? Linker.getNamespace(getClass()) : file;
    }

    public List<FunckyImport> getImports() {
        return imports;
    }

    public List<FunckyDefinition> getDefinitions() {
        return definitions;
    }

    public FunckyDefinition getDefinition(final String name) {
        return definitions.stream()
                .filter(definition -> definition.name().equals(name))
                .findFirst()
                .orElse(null);
    }

    public Set<FunckyScript> getDependencies() {
        return getDependencies(new HashSet<>());
    }

    @Override
    public FunckyEngine getEngine() {
        return engine;
    }

    @Override
    public FunckyNumber eval(final ScriptContext context) {
            return (FunckyNumber) new FunckyApplication(new FunckyReference(engine, getFile(), -1, -1, getFile(), MAIN),
                    new FunckyLiteral(new FunckyJavaConverter().convert(Arrays.asList(engine.getManager().getArguments())))).eval(
                    context);
    }

    @Override
    public FunckyNumber eval() {
        return eval((engine == null) ? new SimpleScriptContext() : engine.getContext());
    }

    @Override
    public String toString() {
        return getFile().toString();
    }

    private Set<FunckyScript> getDependencies(final Set<URI> visited) {
        if (visited.contains(getFile())) {
            return Set.of();
        }
        final Set<FunckyScript> dependencies = new HashSet<>();
        definitions.stream()
                .map(FunckyDefinition::getDependencies)
                .flatMap(Set::stream)
                .map(engine.getManager()::getScript)
                .forEach(dependencies::add);
        visited.add(getFile());
        for (final FunckyScript dependency : Set.copyOf(dependencies)) {
            dependencies.addAll(dependency.getDependencies(visited));
        }
        dependencies.add(this);
        return dependencies;
    }
}
