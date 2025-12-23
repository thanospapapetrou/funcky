package io.github.thanospapapetrou.funcky;

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

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyScope;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public class FunckyContext implements ScriptContext {
    public static final FunckyContext GLOBAL = new FunckyContext();

    private static final String DEFINITION = "%1$s$definition$%2$s$expression";
    private static final String ERROR = "%1$s$definition$%2$s$error";
    private static final String ERROR_RETRIEVING_ATTRIBUTE_SCOPE = "Retrieving attribute scope is not supported";
    private static final String ERROR_RETRIEVING_ATTRIBUTE_WITHOUT_SCOPE =
            "Retrieving attribute without scope is not supported";
    private static final String TYPE = "%1$s$definition$%2$s$type";
    private static final String VALUE = "%1$s$definition$%2$s$value";

    private final Map<Integer, Bindings> bindings;
    private Reader reader;
    private Writer writer;
    private Writer errorWriter;

    static {
        GLOBAL.setBindings(new SimpleBindings(), GLOBAL_SCOPE);
    }

    FunckyContext() {
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
        return (Integer) getAttribute(script.toString(), GLOBAL_SCOPE);
    }

    public void setScript(final URI script) {
        final int base = getScopes().stream().max(Comparator.naturalOrder()).orElse(-1) + 1;
        setAttribute(script.toString(), base, GLOBAL_SCOPE);
        for (final FunckyScope scope : FunckyScope.values()) {
            setBindings(new SimpleBindings(), base + scope.ordinal());
        }
    }

    public FunckyImport getImport(final URI script, final String prefix) {
        return getAttribute(script, FunckyScope.IMPORTS, prefix);
    }

    public void setImport(final FunckyImport inport) {
        setAttribute(inport.file(), FunckyScope.IMPORTS, inport.prefix(), inport);
    }

    public FunckyDefinition getDefinition(final URI script, final String name) {
        return (FunckyDefinition) getAttribute(String.format(DEFINITION, script, name), GLOBAL_SCOPE);
    }

    public void setDefinition(final FunckyDefinition definition) {
        setAttribute(String.format(DEFINITION, definition.file(), definition.name()),
                definition, GLOBAL_SCOPE);
    }

    public FunckyType getType(final FunckyReference reference) {
        return (FunckyType) getAttribute(String.format(TYPE, reference.getCanonical(), reference.getName()),
                GLOBAL_SCOPE);
    }

    public void setType(final URI script, final String name, final FunckyType type) {
        setAttribute(String.format(TYPE, script, name), type, GLOBAL_SCOPE);
    }

    public FunckyValue getValue(final FunckyReference reference) {
        return (FunckyValue) getAttribute(String.format(VALUE, reference.getCanonical(), reference.getName()),
                GLOBAL_SCOPE);
    }

    public void setValue(final URI script, final String name, final FunckyValue value) {
        setAttribute(String.format(VALUE, script, name), value, GLOBAL_SCOPE);
    }

    public FunckyRuntimeException getError(final FunckyReference reference) {
        return (FunckyRuntimeException) getAttribute(String.format(ERROR, reference.getCanonical(),
                        reference.getName()),
                GLOBAL_SCOPE);
    }

    public void setError(final URI script, final String name, final FunckyRuntimeException error) {
        setAttribute(String.format(ERROR, script, name), error, GLOBAL_SCOPE);
    }

    @Override
    public List<Integer> getScopes() {
        return bindings.keySet().stream().sorted().toList();
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

    private <T> T getAttribute(final URI script, final FunckyScope scope, final String name) {
        final Integer base = getScript(script);
        return (T) ((base == null) ? null : getAttribute(name, base + scope.ordinal()));
    }

    private <T> void setAttribute(final URI script, final FunckyScope scope, final String name, final T value) {
        setAttribute(name, value, getScript(script) + scope.ordinal());
    }

}
