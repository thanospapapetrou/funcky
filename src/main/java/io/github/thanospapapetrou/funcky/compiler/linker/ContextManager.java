package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.net.URI;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyFactory;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;

public class ContextManager {
    private static final String DEFINITION_EXPRESSION = "%1$s$definition$%2$s$expression";
    private static final String DEFINITION_TYPE = "%1$s$definition$%2$s$type";
    private static final String IMPORT = "%1$s$import$%2$s";

    private final ScriptContext context;

    public ContextManager(final ScriptContext context) {
        this.context = (context == null) ? FunckyFactory.GLOBAL : context;
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
        return context.getAttribute(script.toString(), ScriptContext.GLOBAL_SCOPE) != null;
    }

    public void setLoaded(final URI script) {
        context.setAttribute(script.toString(), true, ScriptContext.GLOBAL_SCOPE);
    }

    public URI getImport(final URI script, final String prefix) {
        return (URI) context.getAttribute(String.format(IMPORT, script, prefix), ScriptContext.GLOBAL_SCOPE);
    }

    public void setImport(final URI script, final String prefix, final URI namespace) {
        context.setAttribute(String.format(IMPORT, script, prefix), namespace, ScriptContext.GLOBAL_SCOPE);
    }

    public FunckyExpression getDefinitionExpression(final URI namespace, final String name) {
        return (FunckyExpression) context.getAttribute(String.format(DEFINITION_EXPRESSION, namespace, name),
                ScriptContext.GLOBAL_SCOPE);
    }

    public void setDefinitionExpression(final URI script, final String name, final FunckyExpression expression) {
        context.setAttribute(String.format(DEFINITION_EXPRESSION, script, name), expression,
                ScriptContext.GLOBAL_SCOPE);
    }

    public FunckyType getDefinitionType(final URI namespace, final String name) {
        return (FunckyType) context.getAttribute(String.format(DEFINITION_TYPE, namespace, name),
                ScriptContext.GLOBAL_SCOPE);
    }

    public void setDefinitionType(final URI script, final String name, final FunckyType type) {
        context.setAttribute(String.format(DEFINITION_TYPE, script, name), type, ScriptContext.GLOBAL_SCOPE);
    }
}
