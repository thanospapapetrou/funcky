package io.github.thanospapapetrou.funcky.runtime;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyFunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckyFunctionType extends FunckyType {
    private final FunckyExpression domain;
    private final FunckyExpression range;

    public FunckyFunctionType(final FunckyExpression domain, final FunckyExpression range) {
        this.domain = domain;
        this.range = range;
    }

    public FunckyFunctionType(final FunckyType... types) {
        this(Arrays.asList(types));
    }

    private FunckyFunctionType(final List<FunckyType> types) {
        this(new FunckyLiteral(types.get(0)), new FunckyLiteral((types.size() == 2)
                ? types.get(1) : new FunckyFunctionType(types.subList(1, types.size()))));
    }

    public FunckyExpression getDomain() {
        return domain;
    }

    public FunckyExpression getRange() {
        return range;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(new FunckyApplication(Types.FUNCTION.toExpression(), domain), range);
    }

    @Override
    public int compareTo(final FunckyType type) {
        try {
            final int classComparison = super.compareTo(type);
            if (classComparison == 0) {
                final int domainComparison =
                        ((FunckyType) domain.eval()).compareTo((FunckyType) ((FunckyFunctionType) type).domain.eval());
                return (domainComparison == 0) ? ((FunckyType) range.eval()).compareTo(
                        (FunckyType) ((FunckyFunctionType) type).range.eval()) : domainComparison;
            }
            return classComparison;
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    public boolean equals(final Object object) {
        try {
            return (object instanceof FunckyFunctionType) && domain.eval()
                    .equals(((FunckyFunctionType) object).domain.eval())
                    && range.eval().equals(((FunckyFunctionType) object).range.eval());
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    public int hashCode() {
        try {
            return domain.eval().hashCode() + range.eval().hashCode();
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() throws FunckyRuntimeException {
        final Set<FunckyTypeVariable> typeVariables = new HashSet<>(((FunckyType) domain.eval()).getTypeVariables());
        typeVariables.addAll(((FunckyType) range.eval()).getTypeVariables());
        return typeVariables;
    }

    @Override
    protected FunckyFunctionType bind(final Map<FunckyTypeVariable, FunckyType> bindings)
            throws FunckyRuntimeException {
        return new FunckyFunctionType(((FunckyType) domain.eval()).bind(bindings),
                ((FunckyType) range.eval()).bind(bindings));
    }
}
