package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.exceptions.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnboundPrefixException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UndefinedNameException;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyFunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;

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

    public FunckyReference(final Class<? extends FunckyLibrary> library, final String name) {
        this(null, null, -1, -1, Linker.getNamespace(library), name);
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

    public URI resolveNamespace() throws UnboundPrefixException {
        if (namespace == null) {
            if (prefix == null) {
                return file;
            } else {
                final URI namespace = engine.getManager().getImport(file, prefix);
                if (namespace == null) {
                    throw new UnboundPrefixException(this);
                }
                return namespace;
            }
        } else {
            return Linker.normalize(file, namespace);
        }
    }

    @Override
    public FunckyReference normalize() throws UnboundPrefixException {
        return new FunckyReference(engine, file, line, column, resolveNamespace(), name);
    }

    @Override
    public FunckyType getType() throws FunckyCompilationException {
        final URI namespace = resolveNamespace();
        if (engine.getManager().getDefinitionType(namespace, name) == null) {
            engine.getManager().setDefinitionType(namespace, name, super.getType());
        }
        return engine.getManager().getDefinitionType(namespace, name);
    }

    @Override
    public FunckyValue eval(final ScriptContext context) throws FunckyRuntimeException {
        try {
            return resolveExpression().eval(context);
        } catch (final FunckyCompilationException e) {
            throw new FunckyRuntimeException(e);
        } catch (final SneakyFunckyRuntimeException e) {
            throw (FunckyRuntimeException) e.getCause();
        } catch (final FunckyRuntimeException e) {
            e.addStackTrace(this);
            throw e;
        }
    }

    @Override
    public String toString() {
        return (namespace == null) ? ((prefix == null) ? name : String.format(FORMAT_PREFIX, prefix, name))
                : String.format(FORMAT_NAMESPACE, EscapeHelper.escape(namespace.toString()), name);
    }

    @Override
    protected FunckyType getType(final Map<FunckyReference, FunckyTypeVariable> assumptions)
            throws FunckyCompilationException {
        if (assumptions.containsKey(this)) {
            return assumptions.get(this);
        }
        final Map<FunckyReference, FunckyTypeVariable> newAssumptions = new HashMap<>(assumptions);
        newAssumptions.put(this, new FunckyTypeVariable());
        return resolveExpression().getType(newAssumptions);
    }

    private FunckyExpression resolveExpression() throws FunckyCompilationException {
        final URI namespace = resolveNamespace();
        if (!engine.getManager().isLoaded(namespace)) {
            engine.compile(namespace);
        }
        final FunckyExpression expression = engine.getManager().getDefinitionExpression(namespace, name);
        if (expression == null) {
            throw new UndefinedNameException(namespace, this);
        }
        return expression;
    }
}
