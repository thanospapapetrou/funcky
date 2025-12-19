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
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
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
        return new FunckyApplication(new FunckyApplication(new FunckyReference(engine, FunckyLibrary.getNamespace(Types.class), "Function"), // TODO
                domain), range);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        if (value instanceof FunckyFunctionType type) {
            final int domainComparison =
                    domain.eval(engine.getContext()).compareTo(type.domain.eval(engine.getContext()));
            return (domainComparison == 0) ? range.eval(engine.getContext())
                    .compareTo(type.range.eval(engine.getContext())) : domainComparison;
        }
        return super.compareTo(value);
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
