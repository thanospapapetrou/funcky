package io.github.thanospapapetrou.funcky.runtime;

import java.util.Map;
import java.util.Set;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyFunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckyListType extends FunckyType {
    public static final FunckyListType STRING = new FunckyListType(FunckySimpleType.CHARACTER);

    private final FunckyExpression element;

    public FunckyListType(final FunckyExpression element) {
        this.element = element;
    }

    public FunckyListType(final FunckyType element) {
        this(new FunckyLiteral(element));
    }

    public FunckyExpression getElement() {
        return element;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(Types.LIST.toExpression(), element);
    }

    @Override
    public int compareTo(final FunckyType type) {
        try {
            final int classComparison = super.compareTo(type);
            return (classComparison == 0) ? ((FunckyType) element.eval()).compareTo(
                    (FunckyType) ((FunckyListType) type).element.eval()) : classComparison;
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    public boolean equals(final Object object) {
        try {
            return (object instanceof FunckyListType) && element.eval()
                    .equals(((FunckyListType) object).element.eval());
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    public int hashCode() {
        try {
            return element.eval().hashCode();
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() throws FunckyRuntimeException {
        return ((FunckyType) element.eval()).getTypeVariables();
    }

    @Override
    protected FunckyListType bind(final Map<FunckyTypeVariable, FunckyType> bindings)
            throws FunckyRuntimeException {
        return new FunckyListType(((FunckyType) element.eval()).bind(bindings));
    }
}
