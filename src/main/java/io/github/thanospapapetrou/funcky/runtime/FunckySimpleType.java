package io.github.thanospapapetrou.funcky.runtime;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.IntStream;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckySimpleType extends FunckyType {
    public static final FunckySimpleType TYPE = new FunckySimpleType("Type");
    public static final FunckySimpleType NUMBER = new FunckySimpleType("Number");
    public static final FunckySimpleType BOOLEAN = new FunckySimpleType("Boolean");
    public static final FunckySimpleType CHARACTER = new FunckySimpleType("Character");
    private static final List<FunckySimpleType> ORDERING = List.of(TYPE, NUMBER, BOOLEAN, CHARACTER);

    private final String name;

    private static int getOrder(final FunckySimpleType type) {
        return IntStream.range(0, ORDERING.size())
                .filter(i -> ORDERING.get(i).equals(type))
                .findFirst()
                .orElse(-1);
    }

    private FunckySimpleType(final String name) {
        this.name = name;
    }

    @Override
    public FunckyReference toExpression() {
        return new FunckyReference(Types.class, name);
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
