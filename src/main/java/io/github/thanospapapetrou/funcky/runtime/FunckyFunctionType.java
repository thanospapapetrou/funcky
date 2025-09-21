package io.github.thanospapapetrou.funcky.runtime;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckyFunctionType extends FunckyType {
    private final FunckyExpression domain;
    private final FunckyExpression range;

    public FunckyFunctionType(final FunckyEngine engine, final FunckyExpression domain, final FunckyExpression range) {
        super(engine);
        this.domain = domain;
        this.range = range;
    }

    public FunckyFunctionType(final FunckyEngine engine, final FunckyType... types) {
        this(engine, Arrays.asList(types));
    }

    private FunckyFunctionType(final FunckyEngine engine, final List<FunckyType> types) {
        this(engine, new FunckyLiteral(engine, types.get(0)), new FunckyLiteral(engine,
                (types.size() == 2) ? types.get(1) : new FunckyFunctionType(engine, types.subList(1, types.size()))));
    }

    public FunckyExpression getDomain() {
        return domain;
    }

    public FunckyExpression getRange() {
        return range;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(new FunckyApplication(Types.FUNCTION.apply(engine).toExpression(), domain), range);
    }

    @Override
    public int compareTo(final FunckyType type) {
            final int classComparison = super.compareTo(type);
            if (classComparison == 0) {
                final int domainComparison =
                        ((FunckyType) domain.eval()).compareTo((FunckyType) ((FunckyFunctionType) type).domain.eval());
                return (domainComparison == 0) ? ((FunckyType) range.eval()).compareTo(
                        (FunckyType) ((FunckyFunctionType) type).range.eval()) : domainComparison;
            }
            return classComparison;
    }

    @Override
    public boolean equals(final Object object) {
            return (object instanceof FunckyFunctionType) && domain.eval()
                    .equals(((FunckyFunctionType) object).domain.eval())
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
        return new FunckyFunctionType(engine, ((FunckyType) domain.eval()).bind(bindings),
                ((FunckyType) range.eval()).bind(bindings));
    }
}
