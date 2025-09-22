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
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public abstract class HigherOrderFunction extends FunckyFunction {
    private final int order;
    private final FunckyExpression expression;
    private final List<FunckyExpression> arguments;

    HigherOrderFunction(final FunckyEngine engine, final Class<? extends FunckyLibrary> library, final String name,
            final FunckyType... types) {
        this(engine, new FunckyFunctionType(engine, types), types.length - 1,
                new FunckyReference(engine, library, name), List.of());
    } // TODO remove

    HigherOrderFunction(final FunckyEngine engine, final FunckyLibrary library, final String name,
            final FunckyType... types) { // TODO remove name
        this(engine, new FunckyFunctionType(engine, types), types.length - 1,
                new FunckyReference(engine, library.getClass(), name), List.of());
    }

    private HigherOrderFunction(final FunckyEngine engine, final FunckyFunctionType type, final int order,
            final FunckyExpression expression, final List<FunckyExpression> arguments) {
        super(engine, type);
        this.order = order;
        this.expression = expression;
        this.arguments = arguments;
    }

    @Override
    public FunckyValue apply(final FunckyExpression argument, final ScriptContext context) {
            final HigherOrderFunction that = this;
            final FunckyType range = (FunckyType) ((FunckyFunctionType) that.type.unify(
                    new FunckyFunctionType(engine, argument.getType(), new FunckyTypeVariable(engine)))).getRange()
                    .eval();
            final List<FunckyExpression> arguments = new ArrayList<>(this.arguments);
            arguments.add(argument);
        return (order > 1) ? new HigherOrderFunction(engine, (FunckyFunctionType) range, order - 1,
                    new FunckyApplication(that.toExpression(), argument), arguments) {
                @Override
                protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
                    return that.apply(context, arguments);
                }
            } : apply(context, arguments);
    }

    protected abstract FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments);

    @Override
    public FunckyExpression toExpression() {
        return expression;
    }

    // TODO improve
    private static String getName(final FunckyLibrary library, final HigherOrderFunction that) {
        return Arrays.stream(library.getClass().getDeclaredFields())
                .filter(field -> Modifier.isPublic(field.getModifiers()))
                .filter(field -> Modifier.isFinal(field.getModifiers()))
                .filter(field -> {
                    try {
                        return field.get(library) == that;
                    } catch (final IllegalAccessException e) {
                        throw new IllegalStateException("Error accessing field", e);
                    }
                })
                .map(Field::getName)
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("Error resolving field"));
    }
}
