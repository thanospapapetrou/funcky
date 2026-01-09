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

import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER;

public final class FunckyListType extends FunckyType {
    public static final Function<FunckyContext, FunckyListType> STRING = LIST(CHARACTER);

    private final FunckyExpression element;

    public static Function<FunckyContext, FunckyListType> LIST(final Object element) {
        return context -> new FunckyListType(context, new FunckyLiteral(type(element).apply(context)));
    }

    public FunckyListType(final FunckyContext context, final FunckyExpression element) {
        super(context);
        this.element = element;
    }

    public FunckyExpression getElement() {
        return element;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(
                new FunckyReference(context.getEngine(), FunckyLibrary.getNamespace(Types.class), "List"),
                element); // TODO reference list to constant
    }

    @Override
    public int compareTo(final FunckyValue value) {
        return (value instanceof FunckyListType type) ? element.eval(context)
                .compareTo(type.element.eval(context)) : super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return element.eval(context).hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        return ((FunckyType) element.eval(context)).getTypeVariables();
    }

    @Override
    protected FunckyListType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        return LIST(((FunckyType) element.eval(context)).bind(bindings)).apply(context);
    }
}
