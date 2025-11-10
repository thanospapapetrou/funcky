package io.github.thanospapapetrou.funcky.runtime;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import io.github.thanospapapetrou.funcky.compiler.linker.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckyFunctionType extends FunckyType {
    private static final FunckyReference FUNCTION = new FunckyReference(Types.class, "Function");

    private final FunckyExpression domain;
    private final FunckyExpression range;

    public static FunckyFunctionType FUNCTION(final FunckyType... types) {
        return new FunckyFunctionType(new FunckyLiteral(types[0]), new FunckyLiteral((types.length == 2) ? types[1]
                        : FUNCTION(Arrays.copyOfRange(types, 1, types.length))));
    }

    public FunckyFunctionType(final FunckyExpression domain, final FunckyExpression range) {
        this.domain = domain;
        this.range = range;
    }

    public FunckyExpression getDomain() {
        return domain;
    }

    public FunckyExpression getRange() {
        return range;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(new FunckyApplication(FUNCTION, domain), range);
    }

    @Override
    public int compareTo(final FunckyType type) {
            final int classComparison = super.compareTo(type);
            if (classComparison == 0) {
                final int domainComparison = ((FunckyType) domain.eval())
                                .compareTo((FunckyType) ((FunckyFunctionType) type).domain.eval());
                return (domainComparison == 0)
                        ? ((FunckyType) range.eval()).compareTo((FunckyType) ((FunckyFunctionType) type).range.eval())
                        : domainComparison;
            }
            return classComparison;
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyFunctionType)
                && domain.eval().equals(((FunckyFunctionType) object).domain.eval())
                && range.eval().equals(((FunckyFunctionType) object).range.eval());
    }

    @Override
    public int hashCode() {
        return domain.eval().hashCode() + range.eval().hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        final Set<FunckyTypeVariable> typeVariables = new HashSet<>(((FunckyType) domain.eval()).getTypeVariables());
        typeVariables.addAll(((FunckyType) range.eval()).getTypeVariables());
        return typeVariables;
    }

    @Override
    protected FunckyFunctionType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        return FUNCTION(((FunckyType) domain.eval()).bind(bindings),
                ((FunckyType) range.eval()).bind(bindings));
    }
}
