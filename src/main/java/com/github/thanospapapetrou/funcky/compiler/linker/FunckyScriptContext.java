package com.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.net.URI;

import javax.script.ScriptContext;
import javax.script.SimpleScriptContext;

import com.github.thanospapapetrou.funcky.FunckyEngine;
import com.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public class FunckyScriptContext extends SimpleScriptContext {
    private static final String DEFINITION_EXPRESSION = "%1$s$definition$%2$s$expression";
    private static final String DEFINITION_TYPE = "%1$s$definition$%2$s$type";
    private static final String IMPORT = "%1$s$import$%2$s";

    public static FunckyScriptContext getContext(final ScriptContext context) {
        return (context instanceof FunckyScriptContext) ? ((FunckyScriptContext) context)
                : new FunckyScriptContext(context);
    }

    private FunckyScriptContext(final ScriptContext context) {
        super();
        setReader(context.getReader());
        setWriter(context.getWriter());
        setErrorWriter(context.getErrorWriter());
        setBindings(context.getBindings(GLOBAL_SCOPE), GLOBAL_SCOPE);
        setBindings(context.getBindings(ENGINE_SCOPE), ENGINE_SCOPE);
    }

    public URI getFile() throws IOException {
        return new File((String) getAttribute(FunckyEngine.FILENAME, ScriptContext.ENGINE_SCOPE)).getCanonicalFile()
                .toURI();
    }

    public void setFile(final String file) {
        setAttribute(FunckyEngine.FILENAME, file, ScriptContext.ENGINE_SCOPE);
    }

    public String[] getArguments() {
        return (String[]) getAttribute(FunckyEngine.ARGV, ScriptContext.ENGINE_SCOPE);
    }

    public void setArguments(final String... arguments) {
        setAttribute(FunckyEngine.ARGV, arguments, ScriptContext.ENGINE_SCOPE);
    }

    public boolean isLoaded(final URI script) {
        return getAttribute(script.toString(), ScriptContext.ENGINE_SCOPE) != null;
    }

    public void setLoaded(final URI script) {
        setAttribute(script.toString(), true, ScriptContext.ENGINE_SCOPE);
    }

    public URI getImport(final URI script, final String prefix) {
        return (URI) getAttribute(String.format(IMPORT, script, prefix), ScriptContext.ENGINE_SCOPE);
    }

    public void setImport(final URI script, final String prefix, final URI namespace) {
        setAttribute(String.format(IMPORT, script, prefix), namespace, ScriptContext.ENGINE_SCOPE);
    }

    public FunckyExpression getDefinitionExpression(final URI namespace, final String name) {
        return (FunckyExpression) getAttribute(String.format(DEFINITION_EXPRESSION, namespace, name),
                ScriptContext.ENGINE_SCOPE);
    }

    public void setDefinitionExpression(final URI script, final String name, final FunckyExpression expression) {
        setAttribute(String.format(DEFINITION_EXPRESSION, script, name), expression, ScriptContext.ENGINE_SCOPE);
    }

    public FunckyType getDefinitionType(final URI namespace, final String name) {
        return (FunckyType) getAttribute(String.format(DEFINITION_TYPE, namespace, name), ScriptContext.ENGINE_SCOPE);
    }

    public void setDefinitionType(final URI script, final String name, final FunckyType type) {
        setAttribute(String.format(DEFINITION_TYPE, script, name), type, ScriptContext.ENGINE_SCOPE);
    }
}
