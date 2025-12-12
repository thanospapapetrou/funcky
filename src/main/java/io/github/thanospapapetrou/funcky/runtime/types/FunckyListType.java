package io.github.thanospapapetrou.funcky.runtime.types;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER;

public final class FunckyListType extends FunckyType {
    public static final Function<FunckyEngine, FunckyListType> STRING = LIST(CHARACTER);

    private final FunckyExpression element;

    public static Function<FunckyEngine, FunckyListType> LIST(
            final Function<FunckyEngine, ? extends FunckyType> element) {
        return engine -> new FunckyListType(engine, new FunckyLiteral(engine, element.apply(engine)));
    }

    public FunckyListType(final FunckyEngine engine, final FunckyExpression element) {
        super(engine);
        this.element = element;
    }

    public FunckyExpression getElement() {
        return element;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(new Types(engine).$List.toExpression(), element);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        return (value instanceof FunckyListType type) ? element.eval(engine.getContext())
                .compareTo(type.element.eval(engine.getContext())) : super.compareTo(value);
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyListType type) && (compareTo(type) == 0);
    }

    @Override
    public int hashCode() {
        return element.eval(engine.getContext()).hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        return ((FunckyType) element.eval(engine.getContext())).getTypeVariables();
    }

    @Override
    protected FunckyListType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        return new FunckyListType(engine, new FunckyLiteral(engine, ((FunckyType) element.eval(engine.getContext())).bind(bindings)));
    }
}
