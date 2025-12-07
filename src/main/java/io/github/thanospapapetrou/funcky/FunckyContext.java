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
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyScope;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public class FunckyContext implements ScriptContext {
    public static final FunckyContext GLOBAL = new FunckyContext();

    private static final String DEFINITION_EXPRESSION = "%1$s$definition$%2$s$expression";
    private static final String DEFINITION_TYPE = "%1$s$definition$%2$s$type";
    private static final String ERROR_RETRIEVING_ATTRIBUTE_SCOPE = "Retrieving attribute scope is not supported";
    private static final String ERROR_RETRIEVING_ATTRIBUTE_WITHOUT_SCOPE =
            "Retrieving attribute without scope is not supported";
    private static final String IMPORT = "%1$s$import$%2$s";

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

    public URI getImport(final FunckyReference reference) {
        return getAttribute(reference.getFile(), FunckyScope.IMPORTS, reference.getPrefix());
    }

    public void setImport(final FunckyImport inport, final URI namespace) {
        setAttribute(inport.file(), FunckyScope.IMPORTS, inport.prefix(), namespace);
    }

    public FunckyExpression getDefinitionExpression(final FunckyReference reference) {
        return (FunckyExpression) getAttribute(String.format(DEFINITION_EXPRESSION, reference.getNamespace(),
                reference.getName()), GLOBAL_SCOPE);
    }

    public void setDefinitionExpression(final FunckyDefinition definition) {
        setAttribute(String.format(DEFINITION_EXPRESSION, definition.file(), definition.name()),
                definition.expression(), GLOBAL_SCOPE);
    }

    public FunckyType getDefinitionType(final FunckyReference reference) {
        return (FunckyType) getAttribute(String.format(DEFINITION_TYPE, reference.getNamespace(), reference.getName()),
                GLOBAL_SCOPE);
    }

    public void setDefinitionType(final FunckyReference reference, final FunckyType type) {
        setAttribute(String.format(DEFINITION_TYPE, reference.getNamespace(), reference.getName()), type, GLOBAL_SCOPE);
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
