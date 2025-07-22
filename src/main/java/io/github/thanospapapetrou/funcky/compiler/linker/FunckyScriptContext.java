package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import javax.script.ScriptContext;
import javax.script.SimpleScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public class FunckyScriptContext extends SimpleScriptContext { // TODO get rid of this class
    private static final Map<String, Object> CONTEXT = new HashMap<>();
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

    public static boolean isLoaded(final URI script) {
        return CONTEXT.get(script.toString()) != null;
    }

    public static void setLoaded(final URI script) {
        CONTEXT.put(script.toString(), true);
    }

    public static URI getImport(final URI script, final String prefix) {
        return (URI) CONTEXT.get(String.format(IMPORT, script, prefix));
    }

    public static void setImport(final URI script, final String prefix, final URI namespace) {
        CONTEXT.put(String.format(IMPORT, script, prefix), namespace);
    }

    public static FunckyExpression getDefinitionExpression(final URI namespace, final String name) {
        return (FunckyExpression) CONTEXT.get(String.format(DEFINITION_EXPRESSION, namespace, name));
    }

    public static void setDefinitionExpression(final URI script, final String name, final FunckyExpression expression) {
        CONTEXT.put(String.format(DEFINITION_EXPRESSION, script, name), expression);
    }

    public static FunckyType getDefinitionType(final URI namespace, final String name) {
        return (FunckyType) CONTEXT.get(String.format(DEFINITION_TYPE, namespace, name));
    }

    public static void setDefinitionType(final URI script, final String name, final FunckyType type) {
        CONTEXT.put(String.format(DEFINITION_TYPE, script, name), type);
    }
}
