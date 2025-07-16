package com.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import javax.script.ScriptContext;

import com.github.thanospapapetrou.funcky.FunckyEngine;
import com.github.thanospapapetrou.funcky.FunckyFactory;
import com.github.thanospapapetrou.funcky.compiler.CompilationException;
import com.github.thanospapapetrou.funcky.compiler.linker.FunckyScriptContext;
import com.github.thanospapapetrou.funcky.compiler.linker.Linker;
import com.github.thanospapapetrou.funcky.compiler.linker.exceptions.UnboundPrefixException;
import com.github.thanospapapetrou.funcky.compiler.linker.exceptions.UndefinedNameException;
import com.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import com.github.thanospapapetrou.funcky.runtime.FunckyValue;
import com.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import com.github.thanospapapetrou.funcky.runtime.exceptions.SneakyFunckyRuntimeException;
import com.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public class FunckyReference extends FunckyExpression {
    private static final String NAMESPACE_REFERENCE = "\"%1$s\".%2$s";
    private static final String PREFIX_REFERENCE = "%1$s.%2$s";

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
        this(FunckyFactory.ENGINE, null, -1, -1, Linker.getNamespace(library), name);
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

    public URI resolveNamespace(final ScriptContext context) throws UnboundPrefixException {
        if (namespace == null) {
            if (prefix == null) {
                return file;
            } else {
                final URI namespace = FunckyScriptContext.getContext(context).getImport(file, prefix);
                if (namespace == null) {
                    throw new UnboundPrefixException(this);
                }
                return namespace;
            }
        } else {
            return engine.getLinker().normalize(file, namespace);
        }
    }

    @Override
    public FunckyReference normalize() throws UnboundPrefixException {
        return new FunckyReference(engine, file, line, column, resolveNamespace(engine.getContext()), name);
    }

    @Override
    public FunckyType getType() throws CompilationException {
        final URI namespace = resolveNamespace(engine.getContext());
        if (engine.getContext().getDefinitionType(namespace, name) == null) {
            engine.getContext().setDefinitionType(namespace, name, super.getType());
        }
        return engine.getContext().getDefinitionType(namespace, name);
    }

    @Override
    public FunckyValue eval(final ScriptContext context) throws FunckyRuntimeException {
        try {
            return resolveExpression(context).eval(context);
        } catch (final CompilationException e) {
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
        return (namespace == null)
                ? ((prefix == null) ? name : String.format(PREFIX_REFERENCE, prefix, name))
                : String.format(NAMESPACE_REFERENCE, EscapeHelper.escape(namespace.toString()), name);
    }

    @Override
    protected FunckyType getType(final Map<FunckyReference, FunckyTypeVariable> assumptions)
            throws CompilationException {
        if (assumptions.containsKey(this)) {
            return assumptions.get(this);
        }
        final Map<FunckyReference, FunckyTypeVariable> newAssumptions = new HashMap<>(assumptions);
        newAssumptions.put(this, new FunckyTypeVariable());
        return resolveExpression(engine.getContext()).getType(newAssumptions);
    }

    private FunckyExpression resolveExpression(final ScriptContext context) throws CompilationException {
        final URI namespace = resolveNamespace(context);
        if (!FunckyScriptContext.getContext(context).isLoaded(namespace)) {
            engine.compile(namespace);
        }
        final FunckyExpression expression =
                FunckyScriptContext.getContext(context).getDefinitionExpression(namespace, name);
        if (expression == null) {
            throw new UndefinedNameException(namespace, this);
        }
        return expression;
    }
}
