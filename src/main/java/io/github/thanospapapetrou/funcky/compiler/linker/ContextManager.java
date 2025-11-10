package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;

public class ContextManager {
    private static final String DEFINITION_TYPE = "%1$s$definition$%2$s$type";

    private final ScriptContext context;

    public ContextManager(final ScriptContext context) {
        this.context = context;
    }

    public URI getFile() throws IOException {
        final String file = (String) context.getAttribute(FunckyEngine.FILENAME, ScriptContext.ENGINE_SCOPE);
        try {
            final URI uri = new URI(file);
            if (uri.isAbsolute()) {
                return uri.normalize();
            }
        } catch (final URISyntaxException e) {
        }
        return new File(file).getCanonicalFile().toURI();
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

    public FunckyScript getScript(final URI namespace) {
        return (FunckyScript) context.getAttribute(namespace.toString(), ScriptContext.ENGINE_SCOPE);
    }

    public void setScript(final FunckyScript script) {
        context.setAttribute(script.getFile().toString(), script, ScriptContext.ENGINE_SCOPE);
    }

    public FunckyType getDefinitionType(final FunckyReference reference) {
        return (FunckyType) context.getAttribute(String.format(DEFINITION_TYPE, reference.getNamespace(),
                        reference.getName()), ScriptContext.ENGINE_SCOPE);
    }

    public void setDefinitionType(final FunckyReference reference, final FunckyType type) {
        context.setAttribute(String.format(DEFINITION_TYPE, reference.getNamespace(), reference.getName()), type,
                ScriptContext.ENGINE_SCOPE);
    }
}
