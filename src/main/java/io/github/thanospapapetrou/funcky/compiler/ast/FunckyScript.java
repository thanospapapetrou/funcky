package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.script.CompiledScript;
import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;

public class FunckyScript extends CompiledScript {
    public static final String MAIN = "main";

    private static final String JAVA = """
            package %1$s;
            
            public class %2$s extends %3$s {
                public static void main(final String[] arguments) {
                    System.out.println("%2$s was here");
                    System.exit(5);
                }
            
                public %2$s(io.github.thanospapapetrou.funcky.FunckyEngine engine) {
                    %4$s
                }
            %5$s}
            """;
    private static final String JAVA_CONSTRUCTOR_LIBRARY = "super(engine);";
    private static final String JAVA_CONSTRUCTOR_SCRIPT = "super(engine, java.net.URI.create(\"%1$s\"));"; // TODO
    // does this need escaping?

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

    public String toJava(final String packadze, final String simpleName, final Class<? extends FunckyScript> parent) {
        return String.format(JAVA, packadze, simpleName, parent.getName(),
                (parent == FunckyScript.class) ? String.format(JAVA_CONSTRUCTOR_SCRIPT, file)
                        : JAVA_CONSTRUCTOR_LIBRARY, definitions.stream()
                        .filter(definition -> definition.line() != -1)
                        .map(FunckyDefinition::toJava)
                        .collect(Collectors.joining()));
    }

    @Override
    public FunckyEngine getEngine() {
        return engine;
    }

    @Override
    public FunckyNumber eval(final ScriptContext context) {
            return (FunckyNumber) new FunckyApplication(new FunckyReference(engine, getFile(), -1, -1, getFile(), MAIN),
                    new FunckyLiteral(engine,
                            engine.getConverter().convert(Arrays.asList(engine.getManager().getArguments())))).eval(
                    context);
    }
}
