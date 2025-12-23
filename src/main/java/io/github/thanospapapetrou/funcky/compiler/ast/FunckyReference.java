package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public final class FunckyReference extends FunckyExpression {
    private static final String FORMAT_NAMESPACE = "\"%1$s\".%2$s";
    private static final String FORMAT_PREFIX = "%1$s.%2$s";

    private final URI namespace;
    private final String prefix;
    private final URI canonical;
    private final String name;

    public FunckyReference(final FunckyEngine engine, final URI file, final int line, final int column,
            final URI namespace, final String name) {
        this(engine, file, line, column, namespace, null, null, name);
    }

    public FunckyReference(final FunckyEngine engine, final URI file, final int line, final int column,
            final String prefix, final String name) {
        this(engine, file, line, column, null, prefix, null, name);
    }

    public FunckyReference(final FunckyEngine engine, final URI file, final int line, final int column,
            final String name) {
        this(engine, file, line, column, null, null, null, name);
    }

    public FunckyReference(final FunckyEngine engine, final URI namespace, final String name) {
        this(engine, null, -1, -1, namespace, null, namespace, name);
    }

    public FunckyReference(final FunckyEngine engine, final URI file, final int line, final int column,
            final URI namespace, final String prefix, final URI canonical, final String name) {
        super(engine, file, line, column);
        this.namespace = namespace;
        this.prefix = prefix;
        this.canonical = canonical;
        this.name = name;
    }

    public URI getNamespace() {
        return namespace;
    }

    public String getPrefix() {
        return prefix;
    }

    public URI getCanonical() {
        return canonical;
    }

    public String getName() {
        return name;
    }

    @Override
    public FunckyType getType() {
        if (engine.getContext().getType(this) == null) {
            engine.getContext().setType(canonical, name, new FunckyTypeVariable(engine));
            engine.getContext().setType(canonical, name,
                    engine.getContext().getDefinition(canonical, name).expression().getType());
        }
        return engine.getContext().getType(this);
    }

    @Override
    public FunckyValue eval(final ScriptContext context) {
        try {
            return engine.getContext().getDefinition(canonical, name).expression().eval(context);
        } catch (final SneakyRuntimeException e) {
            e.getCause().getStack().add(this);
            throw e;
        }
    }

    @Override
    public String toString(final boolean canonical) {
        return ((!canonical) || (this.canonical == null)) ? ((namespace == null) ? ((prefix == null) ? name
                : String.format(FORMAT_PREFIX, prefix, name))
                : String.format(FORMAT_NAMESPACE, EscapeHelper.escape(namespace.toString()), name))
                : String.format(FORMAT_NAMESPACE, EscapeHelper.escape(this.canonical.toString()), name);
    }
}
