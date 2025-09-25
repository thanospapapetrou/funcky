package io.github.thanospapapetrou.funcky.runtime;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.IntStream;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckySimpleType extends FunckyType {
    public static final Function<FunckyEngine, FunckySimpleType> TYPE = engine -> new FunckySimpleType(engine, "Type");
    public static final Function<FunckyEngine, FunckySimpleType> NUMBER = engine -> new FunckySimpleType(engine, "Number");
    public static final Function<FunckyEngine, FunckySimpleType> BOOLEAN = engine -> new FunckySimpleType(engine, "Boolean");
    public static final Function<FunckyEngine, FunckySimpleType> CHARACTER = engine -> new FunckySimpleType(engine, "Character");

    private static final List<String> ORDERING = List.of("Type", "Number", "Boolean", "Character");

    private final String name;

    private static int getOrder(final FunckySimpleType type) {
        return IntStream.range(0, ORDERING.size())
                .filter(i -> ORDERING.get(i).equals(type.name))
                .findFirst()
                .orElse(-1);
    }

    private FunckySimpleType(final FunckyEngine engine, final String name) {
        super(engine);
        this.name = name;
    }

    @Override
    public FunckyReference toExpression() {
        return new FunckyReference(engine, new Types(engine).getFile(), name);
    }

    @Override
    public int compareTo(final FunckyType type) {
        final int classComparison = super.compareTo(type);
        return (classComparison == 0) ? Integer.compare(getOrder(this), getOrder((FunckySimpleType) type))
                : classComparison;
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckySimpleType) && name.equals(((FunckySimpleType) object).name);
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
