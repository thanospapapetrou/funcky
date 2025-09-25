package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.net.URI;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;

public class ContextManager {
    private static final String DEFINITION_EXPRESSION = "%1$s$definition$%2$s$expression";
    private static final String DEFINITION_TYPE = "%1$s$definition$%2$s$type";
    private static final String IMPORT = "%1$s$import$%2$s";

    private final ScriptContext context;

    public ContextManager(final ScriptContext context) {
        this.context = context;
    }

    public URI getFile() throws IOException {
        return new File((String) context.getAttribute(FunckyEngine.FILENAME, ScriptContext.ENGINE_SCOPE))
                .getCanonicalFile().toURI();
    }

    public void setFile(final String file) {
        context.setAttribute(FunckyEngine.FILENAME, file, ScriptContext.ENGINE_SCOPE);
    }

    public String[] getArguments() {
        return (String[]) context.getAttribute(FunckyEngine.ARGV, ScriptContext.ENGINE_SCOPE);
    }

    public void setArguments(final String... arguments) {
        context.setAttribute(FunckyEngine.ARGV, arguments, ScriptContext.ENGINE_SCOPE);
    }

    public boolean isLoaded(final URI script) {
        return context.getAttribute(script.toString(), ScriptContext.ENGINE_SCOPE) != null;
    }

    public void setLoaded(final URI script) {
        context.setAttribute(script.toString(), true, ScriptContext.ENGINE_SCOPE);
    }

    public URI getImport(final URI script, final String prefix) {
        return (URI) context.getAttribute(String.format(IMPORT, script, prefix), ScriptContext.ENGINE_SCOPE);
    }

    public void setImport(final URI script, final String prefix, final URI namespace) {
        context.setAttribute(String.format(IMPORT, script, prefix), namespace, ScriptContext.ENGINE_SCOPE);
    }

    public FunckyExpression getDefinitionExpression(final FunckyReference reference) {
        return (FunckyExpression) context.getAttribute(String.format(DEFINITION_EXPRESSION, reference.getNamespace(), reference.getName()),
                ScriptContext.ENGINE_SCOPE);
    }

    public void setDefinitionExpression(final FunckyDefinition definition) {
        context.setAttribute(String.format(DEFINITION_EXPRESSION, definition.file(), definition.name()), definition.expression(),
                ScriptContext.ENGINE_SCOPE);
    }

    public FunckyType getDefinitionType(final FunckyReference reference) {
        return (FunckyType) context.getAttribute(String.format(DEFINITION_TYPE, reference.getNamespace(), reference.getName()),
                ScriptContext.ENGINE_SCOPE);
    }

    public void setDefinitionType(final FunckyReference reference, final FunckyType type) {
        context.setAttribute(String.format(DEFINITION_TYPE, reference.getNamespace(), reference.getName()), type,
                ScriptContext.ENGINE_SCOPE);
    }
}
