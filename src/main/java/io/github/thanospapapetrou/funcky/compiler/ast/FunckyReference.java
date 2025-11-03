package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyFactory;
import io.github.thanospapapetrou.funcky.compiler.exceptions.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnboundPrefixException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UndefinedNameException;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.compiler.transpiler.Transpiler;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public final class FunckyReference extends FunckyExpression {
    private static final String FORMAT_NAMESPACE = "\"%1$s\".%2$s";
    private static final String FORMAT_PREFIX = "%1$s.%2$s";
    private static final String JAVA = "new %1$s(engine, %2$s.%3$s%4$s)";

    private final URI namespace;
    private final String prefix;
    private final String name;

    public FunckyReference(final FunckyEngine engine, final URI file, final int line, final int column,
            final URI namespace, final String name) {
        this(engine, file, line, column, namespace, null, name);
    }

    public FunckyReference(final FunckyEngine engine, final URI file, final int line, final int column,
            final String prefix, final String name) {
        this(engine, file, line, column, null, prefix, name);
    }

    public FunckyReference(final FunckyEngine engine, final URI file, final int line, final int column,
            final String name) {
        this(engine, file, line, column, null, null, name);
    }

    public FunckyReference(final FunckyEngine engine, final URI namespace, final String name) {
        this(engine, null, -1, -1, namespace, null, name);
    }// TODO remove

    public FunckyReference(final URI namespace, final String name) {
        this(null, null, -1, -1, namespace, null, name);
    }

    private FunckyReference(final FunckyEngine engine, final URI file, final int line, final int column,
            final URI namespace, final String prefix, final String name) {
        super(engine, file, line, column);
        this.namespace = namespace;
        this.prefix = prefix;
        this.name = name;
    }

    public URI getNamespace() {
        return namespace;
    }

    public String getPrefix() {
        return prefix;
    }

    public String getName() {
        return name;
    }

    @Override
    public FunckyType getType() {
        final FunckyReference canonical = canonicalize();
        if (engine.getManager().getDefinitionType(canonical) == null) {
            engine.getManager().setDefinitionType(canonical, super.getType());
        }
        return engine.getManager().getDefinitionType(canonical);
    }

    @Override
    public FunckyReference canonicalize() {
        return new FunckyReference(engine, file, line, column, resolveNamespace(), name);
    }

    @Override
    public String toJava() {
        return String.format(JAVA, FunckyLiteral.class.getName(),
                engine.getTranspiler().getClass(canonicalize().namespace), Transpiler.PREFIX_FUNCKY, name);
    }

    @Override
    public Set<URI> getDependencies() {
        return Set.of(canonicalize().getNamespace());
    }

    @Override
    public FunckyValue eval(final ScriptContext context) {
        try {
            return resolveExpression().eval(context);
        } catch (final SneakyRuntimeException e) {
            e.getCause().getStack().add(this);
            throw e;
        }
    }

    @Override
    public String toString() {
        return (namespace == null) ? ((prefix == null) ? name : String.format(FORMAT_PREFIX, prefix, name))
                : String.format(FORMAT_NAMESPACE, EscapeHelper.escape(namespace.toString()), name);
    }

    @Override
    protected FunckyType getType(final Map<FunckyReference, FunckyTypeVariable> assumptions) {
        if (assumptions.containsKey(this)) {
            return assumptions.get(this);
        }
        final Map<FunckyReference, FunckyTypeVariable> newAssumptions = new HashMap<>(assumptions);
        newAssumptions.put(this, new FunckyTypeVariable());
        return resolveExpression().getType(newAssumptions);
    }

    private URI resolveNamespace() {
        if (namespace == null) {
            if (prefix == null) {
                return file;
            } else {
                final FunckyImport inport = engine.getManager().getScript(file).getImport(prefix);
                if (inport == null) {
                    throw new SneakyCompilationException(new UnboundPrefixException(this));
                }
                return inport.namespace();
            }
        } else {
            return (engine == null) ? null : engine.getLinker().canonicalize(file, namespace);
        }
    }

    private FunckyExpression resolveExpression() {
        final FunckyReference canonical = canonicalize();
        if (engine.getManager().getScript(canonical.getNamespace()) == null) {
            try {
                engine.compile(canonical.getNamespace());
            } catch (final FunckyCompilationException e) {
                throw new SneakyCompilationException(e);
            }
        }
        final FunckyDefinition definition = engine.getManager().getScript(canonical.namespace).getDefinition(name);
        if (definition == null) {
            throw new SneakyCompilationException(new UndefinedNameException(canonical));
        }
        return definition.expression();
    }
}
