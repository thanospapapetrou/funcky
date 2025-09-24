package io.github.thanospapapetrou.funcky.runtime;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckyListType extends FunckyType {
    public static final Function<FunckyEngine, FunckyListType> STRING =
            engine -> new FunckyListType(engine, FunckySimpleType.CHARACTER.apply(engine));

    private final FunckyExpression element;

    public FunckyListType(final FunckyEngine engine, final FunckyExpression element) {
        super(engine);
        this.element = element;
    }

    public FunckyListType(final FunckyEngine engine, final FunckyType element) {
        this(engine, new FunckyLiteral(engine, element));
    }

    public FunckyExpression getElement() {
        return element;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(new Types(engine).$List.toExpression(), element);
    }

    @Override
    public int compareTo(final FunckyType type) {
            final int classComparison = super.compareTo(type);
            return (classComparison == 0) ? ((FunckyType) element.eval()).compareTo(
                    (FunckyType) ((FunckyListType) type).element.eval()) : classComparison;
    }

    @Override
    public boolean equals(final Object object) {
            return (object instanceof FunckyListType) && element.eval()
                    .equals(((FunckyListType) object).element.eval());
    }

    @Override
    public int hashCode() {
            return element.eval().hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        return ((FunckyType) element.eval()).getTypeVariables();
    }

    @Override
    protected FunckyListType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        return new FunckyListType(engine, ((FunckyType) element.eval()).bind(bindings));
    }
}
