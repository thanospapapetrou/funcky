package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.Optional;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.exceptions.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UndefinedNameException;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;

public final class FunckyReference extends FunckyExpression {
    private static final String FORMAT_NAMESPACE = "\"%1$s\".%2$s";
    private static final String FORMAT_PREFIX = "%1$s.%2$s";

    private final URI namespace;
    private final String prefix;
    private final String name;

    public FunckyReference(final FunckyEngine engine, final URI file, final int line, final int column,
            final URI namespace, final String prefix, final String name, final FunckyType type) {
        super(engine, file, line, column, type);
        this.namespace = namespace;
        this.prefix = prefix;
        this.name = name;
    }

    public FunckyReference(final Class<? extends FunckyLibrary> library, final String name) { // TODO add type
        this(null, null, -1, -1, Linker.getNamespace(library), null, name, null);
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

    public FunckyDefinition resolve() {
        if (engine.getManager().getScript(namespace) == null) {
            try {
                engine.compile(namespace);
            } catch (final FunckyCompilationException e) {
                throw new SneakyCompilationException(e);
            }
        }
        final Optional<FunckyDefinition> definition = engine.getManager().getScript(namespace).getDefinitions().stream()
                .filter(def -> def.name().equals(name))
                .findFirst();
        if (definition.isEmpty()) {
            throw new SneakyCompilationException(new UndefinedNameException(this));
        }
        return definition.get();
    }

    @Override
    public FunckyType getType() { // TODO remove
        if (engine.getManager().getDefinitionType(this) == null) {
            engine.getManager().setDefinitionType(this, super.getType());
        }
        return engine.getManager().getDefinitionType(this);
    }

    @Override
    public FunckyValue eval(final ScriptContext context) {
        try {
            return resolve().expression().eval(context);
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
}
