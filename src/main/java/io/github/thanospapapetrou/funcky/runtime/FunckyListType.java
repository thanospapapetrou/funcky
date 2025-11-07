package io.github.thanospapapetrou.funcky.runtime;

import java.util.Map;
import java.util.Set;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.CHARACTER;

public final class FunckyListType extends FunckyType {
    public static final FunckyListType STRING = LIST(CHARACTER);

    private static final FunckyReference LIST = new FunckyReference(Types.class, "List");

    private final FunckyExpression element;

    public static FunckyListType LIST(FunckyType element) {
        return new FunckyListType(new FunckyLiteral(element));
    }

    public FunckyListType(final FunckyExpression element) {
        this.element = element;
    }

    public FunckyExpression getElement() {
        return element;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(LIST, element);
    }

    @Override
    public int compareTo(final FunckyType type) {
        final int classComparison = super.compareTo(type);
        return (classComparison == 0)
                ? ((FunckyType) element.eval()).compareTo((FunckyType) ((FunckyListType) type).element.eval())
                : classComparison;
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyListType) && element.eval().equals(((FunckyListType) object).element.eval());
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
        return LIST(((FunckyType) element.eval()).bind(bindings));
    }
}
