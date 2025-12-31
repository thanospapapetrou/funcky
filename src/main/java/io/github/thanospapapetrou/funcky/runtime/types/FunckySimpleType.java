package io.github.thanospapapetrou.funcky.runtime.types;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.IntStream;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckySimpleType extends FunckyType {
    public static final Function<FunckyContext, FunckySimpleType> TYPE =
            context -> new FunckySimpleType(context, "Type");
    public static final Function<FunckyContext, FunckySimpleType> NUMBER =
            context -> new FunckySimpleType(context, "Number");
    public static final Function<FunckyContext, FunckySimpleType> BOOLEAN =
            context -> new FunckySimpleType(context, "Boolean");
    public static final Function<FunckyContext, FunckySimpleType> CHARACTER =
            context -> new FunckySimpleType(context, "Character");

    private static final List<String> ORDERING = List.of("Type", "Number", "Boolean", "Character");

    private final String name;

    private static int getOrder(final FunckySimpleType type) {
        return IntStream.range(0, ORDERING.size())
                .filter(i -> ORDERING.get(i).equals(type.name))
                .findFirst()
                .orElse(-1);
    }

    private FunckySimpleType(final FunckyContext context, final String name) {
        super(context);
        this.name = name;
    }

    @Override
    public FunckyReference toExpression() {
        return new FunckyReference(context.getEngine(), FunckyLibrary.getNamespace(Types.class), name);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        return (value instanceof FunckySimpleType type) ? Integer.compare(getOrder(this), getOrder(type))
                : super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        return Set.of();
    }

    @Override
    protected FunckySimpleType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        return this;
    }
}
