package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.script.CompiledScript;
import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;

public class FunckyScript extends CompiledScript {
    public static final String MAIN = "main";
    private static final String JAVA = """
                static class %1$s extends %2$s {
                    %1$s(final %3$s engine) {
                        super(engine%4$s);
                    }
            
            %5$s    }
            
                final %1$s %1$s = new %1$s(engine);
                
            """;
    private static final String JAVA_URI = ", %1$s.create(\"%2$s\")";

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

    public String toJava() {
        final Class<? extends FunckyLibrary> parent = engine.getLinker().getLibrary(getFile());
        return String.format(JAVA, engine.getTranspiler().getClass(getFile()),
                ((parent == null) ? FunckyScript.class : parent).getName(), FunckyEngine.class.getName(),
                (parent == null) ? String.format(JAVA_URI, URI.class.getName(),
                        EscapeHelper.escape(getFile().toString())) : "",
                definitions.stream()
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
