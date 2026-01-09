package io.github.thanospapapetrou.funcky.runtime.types;

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

public final class FunckyMonadicType extends FunckyType {
    private static final String IO = "IO";
    private static final String MAYBE = "Maybe";

    private final String name;
    private final FunckyExpression base;

    public static Function<FunckyContext, FunckyMonadicType> MAYBE(
            final Function<FunckyContext, ? extends FunckyType> base) {
        return context -> maybe(context, new FunckyLiteral(base.apply(context)));
    }

    public static Function<FunckyContext, FunckyMonadicType> IO(
            final Function<FunckyContext, ? extends FunckyType> base) {
        return context -> io(context, new FunckyLiteral(base.apply(context)));
    }

    public static FunckyMonadicType maybe(final FunckyContext context, final FunckyExpression base) {
        return new FunckyMonadicType(context, MAYBE, base);
    }

    public static FunckyMonadicType io(final FunckyContext context, final FunckyExpression base) {
        return new FunckyMonadicType(context, IO, base);
    }

    public FunckyMonadicType(final FunckyContext context, final String name, final FunckyExpression base) {
        super(context);
        this.name = name;
        this.base = base;
    }

    public String getName() {
        return name;
    }

    public FunckyExpression getBase() {
        return base;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(
                new FunckyReference(context.getEngine(), FunckyLibrary.getNamespace(Types.class), name), base);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        if (value instanceof FunckyMonadicType monad) {
            final int comparison = name.compareTo(monad.name);
            return (comparison == 0) ? base.eval(context).compareTo(monad.base.eval(context))
                    : comparison;
        }
        return super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return name.hashCode() + base.eval(context).hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        return ((FunckyType) base.eval(context)).getTypeVariables();
    }

    @Override
    protected FunckyMonadicType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        return new FunckyMonadicType(context, name,
                new FunckyLiteral(((FunckyType) base.eval(context)).bind(bindings)));
    }
}
