package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.exceptions.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnboundPrefixException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UndefinedNameException;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public final class FunckyReference extends FunckyExpression {
    private static final String FORMAT_NAMESPACE = "\"%1$s\".%2$s";
    private static final String FORMAT_PREFIX = "%1$s.%2$s";

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
        final FunckyReference normalized = normalize();
        if (engine.getManager().getDefinitionType(normalized) == null) {
            engine.getManager().setDefinitionType(normalized, super.getType());
        }
        return engine.getManager().getDefinitionType(normalized);
    }

    @Override
    public FunckyReference normalize() {
        return new FunckyReference(engine, file, line, column, resolveNamespace(), name);
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
        newAssumptions.put(this, new FunckyTypeVariable(engine));
        return resolveExpression().getType(newAssumptions);
    }

    private URI resolveNamespace() {
        if (namespace == null) {
            if (prefix == null) {
                return file;
            } else {
                final URI namespace = engine.getManager().getImport(file, prefix);
                if (namespace == null) {
                    throw new SneakyCompilationException(new UnboundPrefixException(this));
                }
                return namespace;
            }
        } else {
            return engine.getLinker().normalize(file, namespace);
        }
    }

    private FunckyExpression resolveExpression() {
        final FunckyReference normalized = normalize();
        if (!engine.getManager().isLoaded(normalized.getNamespace())) {
            try {
                engine.compile(normalized.getNamespace());
            } catch (final FunckyCompilationException e) {
                throw new SneakyCompilationException(e);
            }
        }
        final FunckyExpression expression = engine.getManager().getDefinitionExpression(normalized);
        if (expression == null) {
            throw new SneakyCompilationException(new UndefinedNameException(normalized));
        }
        return expression;
    }
}
