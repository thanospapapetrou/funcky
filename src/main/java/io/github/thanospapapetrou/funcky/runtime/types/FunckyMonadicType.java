package io.github.thanospapapetrou.funcky.runtime.types;

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

public final class FunckyMonadicType extends FunckyType {
    private static final String IO = "IO";
    private static final String MAYBE = "Maybe";

    private final String name;
    private final FunckyExpression base;

    public static Function<FunckyEngine, FunckyMonadicType> maybe(
            final Function<FunckyEngine, ? extends FunckyType> base) {
        return engine -> maybe(engine, new FunckyLiteral(engine, base.apply(engine)));
    }

    public static FunckyMonadicType maybe(final FunckyEngine engine, final FunckyExpression base) {
        return new FunckyMonadicType(engine, MAYBE, base);
    }

    public static Function<FunckyEngine, FunckyMonadicType> io(
            final Function<FunckyEngine, ? extends FunckyType> base) {
        return engine -> io(engine, new FunckyLiteral(engine, base.apply(engine)));
    }

    public static FunckyMonadicType io(final FunckyEngine engine, final FunckyExpression base) {
        return new FunckyMonadicType(engine, IO, base);
    }

    public FunckyMonadicType(final FunckyEngine engine, final String name, final FunckyExpression base) {
        super(engine);
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
        return new FunckyApplication(new FunckyReference(engine, FunckyLibrary.getNamespace(Types.class), name), base);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        if (value instanceof FunckyMonadicType monad) {
            final int comparison = name.compareTo(monad.name);
            return (comparison == 0) ? base.eval(engine.getContext()).compareTo(monad.base.eval(engine.getContext()))
                    : comparison;
        }
        return super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return name.hashCode() + base.eval(engine.getContext()).hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        return ((FunckyType) base.eval(engine.getContext())).getTypeVariables();
    }

    @Override
    protected FunckyMonadicType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        return new FunckyMonadicType(engine, name, new FunckyLiteral(engine,
                ((FunckyType) base.eval(engine.getContext())).bind(bindings)));
    }
}
