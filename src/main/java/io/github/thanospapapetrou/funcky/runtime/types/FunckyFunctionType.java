package io.github.thanospapapetrou.funcky.runtime.types;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckyFunctionType extends FunckyType {
    private final FunckyExpression domain;
    private final FunckyExpression range;

    public static Function<FunckyContext, FunckyFunctionType> FUNCTION(
            final Function<FunckyContext, ? extends FunckyType>... types) {
        return context -> new FunckyFunctionType(context, new FunckyLiteral(context.getEngine(),
                types[0].apply(context)), new FunckyLiteral(context.getEngine(), (types.length == 2)
                ? types[1].apply(context) : FUNCTION(Arrays.copyOfRange(types, 1, types.length)).apply(context)));
    }

    public FunckyFunctionType(final FunckyContext context, final FunckyExpression domain,
            final FunckyExpression range) {
        super(context);
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
        return new FunckyApplication(new FunckyApplication(
                new FunckyReference(context.getEngine(), FunckyLibrary.getNamespace(Types.class), "Function"), // TODO
                domain), range);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        if (value instanceof FunckyFunctionType type) {
            final int domainComparison =
                    domain.eval(context).compareTo(type.domain.eval(context));
            return (domainComparison == 0) ? range.eval(context)
                    .compareTo(type.range.eval(context)) : domainComparison;
        }
        return super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return domain.eval(context).hashCode() + range.eval(context).hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        final Set<FunckyTypeVariable> typeVariables =
                new HashSet<>(((FunckyType) domain.eval(context)).getTypeVariables());
        typeVariables.addAll(((FunckyType) range.eval(context)).getTypeVariables());
        return typeVariables;
    }

    @Override
    protected FunckyFunctionType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        return FUNCTION(context -> ((FunckyType) domain.eval(context)).bind(bindings),
                context -> ((FunckyType) range.eval(context)).bind(bindings)).apply(context);
    }
}
