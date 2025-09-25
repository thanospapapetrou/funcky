package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public abstract class HigherOrderFunction extends FunckyFunction {
    private static final String ERROR_RESOLVING_FIELD = "Error resolving field `%1$s`";
    private static final String ERROR_RESOLVING_NAME = "Error resolving name";

    private final int order;
    private final FunckyLibrary library;
    private final FunckyExpression expression;
    private final List<FunckyExpression> arguments;

    HigherOrderFunction(final FunckyEngine engine, final FunckyLibrary library, final FunckyType... types) {
        this(engine, new FunckyFunctionType(engine, types), types.length - 1, library, null, List.of());
    }

    private HigherOrderFunction(final FunckyEngine engine, final FunckyFunctionType type, final int order,
            final FunckyLibrary library, final FunckyExpression expression, final List<FunckyExpression> arguments) {
        super(engine, type);
        this.order = order;
        this.library = library;
        this.expression = expression;
        this.arguments = arguments;
    }

    @Override
    public FunckyValue apply(final FunckyExpression argument, final ScriptContext context) {
            final HigherOrderFunction that = this;
            final FunckyType range = (FunckyType) ((FunckyFunctionType) that.type.unify(
                    new FunckyFunctionType(engine, argument.getType(), new FunckyTypeVariable(engine)))).getRange()
                    .eval(engine.getContext());
            final List<FunckyExpression> arguments = new ArrayList<>(this.arguments);
            arguments.add(argument);
        return (order > 1) ? new HigherOrderFunction(engine, (FunckyFunctionType) range, order - 1,
                    null, new FunckyApplication(that.toExpression(), argument), arguments) {
                @Override
                protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
                    return that.apply(context, arguments);
                }
            } : apply(context, arguments);
    }

    protected abstract FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments);

    @Override
    public FunckyExpression toExpression() {
        return (expression == null) ? new FunckyReference(engine, library.getFile(), getName()) : expression;
    }

    private String getName() {
        return Arrays.stream(library.getClass().getDeclaredFields())
                .filter(field -> Modifier.isPublic(field.getModifiers()))
                .filter(this::isThis)
                .map(Field::getName).map(name -> name.substring(Linker.JAVA_PREFIX.length()))
                .findAny()
                .orElseThrow(() -> new IllegalStateException(ERROR_RESOLVING_NAME));
    }

    private boolean isThis(final Field field) {
        try {
            return field.get(library) == this;
        } catch (final IllegalAccessException e) {
            throw new IllegalStateException(String.format(ERROR_RESOLVING_FIELD, field.getName()), e);
        }
    }
}
