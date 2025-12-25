package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.URI;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.SimpleBindings;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public class FunckyContext implements ScriptContext {
    public static final int SCRIPTS_SCOPE = 0;
    private static final String ERROR_RETRIEVING_ATTRIBUTE_SCOPE = "Retrieving attribute scope is not supported";
    private static final String ERROR_RETRIEVING_ATTRIBUTE_WITHOUT_SCOPE =
            "Retrieving attribute without scope is not supported";

    private final Map<Integer, Bindings> bindings;
    private Reader reader;
    private Writer writer;
    private Writer errorWriter;

    public FunckyContext() {
        this(new HashMap<>());
        setReader(new InputStreamReader(System.in));
        setWriter(new PrintWriter(System.out, true));
        setErrorWriter(new PrintWriter(System.err, true));
    }

    private FunckyContext(final Map<Integer, Bindings> bindings) {
        this.bindings = bindings;
    }

    public URI getFile() throws IOException {
        return new File((String) getAttribute(FunckyEngine.FILENAME, ENGINE_SCOPE)).getCanonicalFile().toURI();
    }

    public void setFile(final String file) {
        setAttribute(FunckyEngine.FILENAME, file, ENGINE_SCOPE);
    }

    public String[] getArguments() {
        return (String[]) getAttribute(FunckyEngine.ARGV, ENGINE_SCOPE);
    }

    public void setArguments(final String... arguments) {
        setAttribute(FunckyEngine.ARGV, arguments, ENGINE_SCOPE);
    }

    public Integer getScript(final URI script) {
        return (Integer) getAttribute(script.toString(), SCRIPTS_SCOPE);
    }

    public void setScript(final URI script) {
        final int base = getScopes().stream().min(Comparator.naturalOrder()).orElse(0) - 1;
        setAttribute(script.toString(), base, SCRIPTS_SCOPE);
        for (final FunckyScope scope : FunckyScope.values()) {
            setBindings(new SimpleBindings(), base - scope.ordinal());
        }
    }

    public FunckyImport getImport(final URI script, final String prefix) {
        return getAttribute(script, FunckyScope.IMPORTS, prefix);
    }

    public void setImport(final FunckyImport inport) {
        setAttribute(inport.file(), FunckyScope.IMPORTS, inport.prefix(), inport);
    }

    public FunckyDefinition getDefinition(final URI script, final String name) {
        return getAttribute(script, FunckyScope.DEFINITIONS, name);
    }

    public void setDefinition(final FunckyDefinition definition) {
        setAttribute(definition.file(), FunckyScope.DEFINITIONS, definition.name(), definition);
    }

    public FunckyType getType(final URI script, final String name) {
        return getAttribute(script, FunckyScope.TYPES, name);
    }

    public void setType(final URI script, final String name, final FunckyType type) {
        setAttribute(script, FunckyScope.TYPES, name, type);
    }

    public FunckyValue getValue(final FunckyReference reference) {
        return getAttribute(reference.getCanonical(), FunckyScope.VALUES, reference.getName());
    }

    public void setValue(final URI script, final String name, final FunckyValue value) {
        setAttribute(script, FunckyScope.VALUES, name, value);
    }

    public FunckyRuntimeException getError(final FunckyReference reference) {
        return getAttribute(reference.getCanonical(), FunckyScope.ERRORS, reference.getName());
    }

    public void setError(final URI script, final String name, final FunckyRuntimeException error) {
        setAttribute(script, FunckyScope.ERRORS, name, error);
    }

    @Override
    public List<Integer> getScopes() {
        return bindings.keySet().stream().sorted(Comparator.reverseOrder()).toList();
    }

    @Override
    public Bindings getBindings(final int scope) {
        return bindings.get(scope);
    }

    @Override
    public void setBindings(final Bindings bindings, final int scope) {
        this.bindings.put(scope, bindings);
    }

    @Override
    public Object getAttribute(final String name, final int scope) {
        return bindings.get(scope).get(name);
    }

    @Override
    public Object getAttribute(final String name) {
        throw new UnsupportedOperationException(ERROR_RETRIEVING_ATTRIBUTE_WITHOUT_SCOPE);
    }

    @Override
    public void setAttribute(final String name, final Object value, final int scope) {
        bindings.get(scope).put(name, value);
    }

    @Override
    public Object removeAttribute(final String name, final int scope) {
        return bindings.get(scope).remove(name);
    }

    @Override
    public int getAttributesScope(final String name) {
        throw new UnsupportedOperationException(ERROR_RETRIEVING_ATTRIBUTE_SCOPE);
    }

    @Override
    public Reader getReader() {
        return reader;
    }

    @Override
    public void setReader(final Reader reader) {
        this.reader = reader;
    }

    @Override
    public Writer getWriter() {
        return writer;
    }

    @Override
    public void setWriter(final Writer writer) {
        this.writer = writer;
    }

    @Override
    public Writer getErrorWriter() {
        return errorWriter;
    }

    @Override
    public void setErrorWriter(final Writer errrorWriter) {
        this.errorWriter = errrorWriter;
    }

    @Override
    public String toString() { // TODO remove
        final StringBuilder string = new StringBuilder();
        string.append("Global (").append(GLOBAL_SCOPE).append(")\n");
        for (final String name : getBindings(GLOBAL_SCOPE).keySet()) {
            string.append("\t").append(name).append(" ").append(getBindings(GLOBAL_SCOPE).get(name)).append("\n");
        }
        string.append("Engine (").append(ENGINE_SCOPE).append(")\n");
        for (final String name : getBindings(ENGINE_SCOPE).keySet()) {
            string.append("\t").append(name).append(" ").append(getBindings(ENGINE_SCOPE).get(name)).append("\n");
        }
        string.append("Scripts (").append(SCRIPTS_SCOPE).append(")\n");
        for (final String script : getBindings(SCRIPTS_SCOPE).keySet()) {
            string.append("\t").append(script).append(" ").append(getBindings(SCRIPTS_SCOPE).get(script)).append("\n");
        }
        for (final String script : getBindings(SCRIPTS_SCOPE).keySet()) {
            final int base = (int) getBindings(SCRIPTS_SCOPE).get(script);
            for (final FunckyScope scope : FunckyScope.values()) {
                string.append(script).append(" ").append(scope).append(" (").append(base - scope.ordinal())
                        .append(")\n");
                // TODO
                //                for (final String name : getBindings(base - scope.ordinal()).keySet()) {
                //                    string.append("\t").append(name).append(" ").append(getBindings(base - scope.ordinal()).get(name)).append("\n");
                //                }
            }
        }
        return string.toString();
    }

    private <T> T getAttribute(final URI script, final FunckyScope scope, final String name) {
        final Integer base = getScript(script);
        return (T) ((base == null) ? null : getAttribute(name, base - scope.ordinal()));
    }

    private <T> void setAttribute(final URI script, final FunckyScope scope, final String name, final T value) {
        setAttribute(name, value, getScript(script) - scope.ordinal());
    }
}
