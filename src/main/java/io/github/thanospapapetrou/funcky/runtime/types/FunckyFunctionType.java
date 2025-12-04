package io.github.thanospapapetrou.funcky.runtime.types;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckyFunctionType extends FunckyType {
    private final FunckyExpression domain;
    private final FunckyExpression range;

    public static Function<FunckyEngine, FunckyFunctionType> FUNCTION(final Function<FunckyEngine, ?
            extends FunckyType>... types) {
        return engine -> new FunckyFunctionType(engine, new FunckyLiteral(engine, types[0].apply(engine)),
                new FunckyLiteral(engine, (types.length == 2) ? types[1].apply(engine) : FUNCTION(Arrays.copyOfRange(types, 1,
                        types.length)).apply(engine)));
    }

    public FunckyFunctionType(final FunckyEngine engine, final FunckyExpression domain, final FunckyExpression range) {
        super(engine);
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
        return new FunckyApplication(new FunckyApplication(new Types(engine).$Function.toExpression(),
                domain), range);
    }

    @Override
    public int compareTo(final FunckyType type) {
            final int classComparison = super.compareTo(type);
            if (classComparison == 0) {
                final int domainComparison =
                        ((FunckyType) domain.eval(engine.getContext())).compareTo((FunckyType) ((FunckyFunctionType) type).domain.eval(engine.getContext()));
                return (domainComparison == 0) ? ((FunckyType) range.eval(engine.getContext())).compareTo(
                        (FunckyType) ((FunckyFunctionType) type).range.eval(engine.getContext())) : domainComparison;
            }
            return classComparison;
    }

    @Override
    public boolean equals(final Object object) {
            return (object instanceof FunckyFunctionType) && domain.eval(engine.getContext())
                    .equals(((FunckyFunctionType) object).domain.eval(engine.getContext()))
                    && range.eval(engine.getContext()).equals(((FunckyFunctionType) object).range.eval(engine.getContext()));
    }

    @Override
    public int hashCode() {
            return domain.eval(engine.getContext()).hashCode() + range.eval(engine.getContext()).hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        final Set<FunckyTypeVariable> typeVariables = new HashSet<>(((FunckyType) domain.eval(engine.getContext())).getTypeVariables());
        typeVariables.addAll(((FunckyType) range.eval(engine.getContext())).getTypeVariables());
        return typeVariables;
    }

    @Override
    protected FunckyFunctionType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        return new FunckyFunctionType(engine,
                new FunckyLiteral(engine, ((FunckyType) domain.eval(engine.getContext())).bind(bindings)),
                new FunckyLiteral(engine, ((FunckyType) range.eval(engine.getContext())).bind(bindings)));
    }
}
