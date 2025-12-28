package io.github.thanospapapetrou.funcky.runtime.types;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.linker.TypeInferenceContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public sealed abstract class FunckyType extends FunckyValue
        permits FunckySimpleType, FunckyFunctionType, FunckyListType, FunckyRecordType, FunckyMonadicType,
        FunckyTypeVariable {
    private static final List<Class<? extends FunckyType>> ORDERING = List.of(
            FunckySimpleType.class,
            FunckyFunctionType.class,
            FunckyListType.class,
            FunckyRecordType.class,
            FunckyTypeVariable.class
    );

    private static int getOrder(final FunckyType type) {
        return IntStream.range(0, ORDERING.size())
                .filter(i -> ORDERING.get(i).isInstance(type))
                .findFirst()
                .orElse(-1);
    }

    protected FunckyType(final FunckyEngine engine) {
        super(engine);
    }

    public FunckyType free() {
        return bind(getTypeVariables().stream()
                .collect(Collectors.toMap(Function.identity(), typeVariable -> new FunckyTypeVariable(engine))));
    }

    public FunckyType unify(final FunckyType type) {
        final FunckyType free = type.free();
        final TypeInferenceContext context = new TypeInferenceContext();
        if (context.unify(this.free(), free)) {
            final Map<FunckyTypeVariable, FunckyType> bindings = new HashMap<>();
            for (final FunckyTypeVariable typeVariable : free.getTypeVariables()) {
                bindings.put(typeVariable, context.find(typeVariable));
            }
            return free.bind(bindings);
        }
        return null;
    }

    @Override
    public FunckySimpleType getType() {
        return FunckySimpleType.TYPE.apply(engine);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        return (value instanceof FunckyType type) ? Integer.compare(getOrder(this), getOrder(type))
                : super.compareTo(value);
    }

    protected abstract Set<FunckyTypeVariable> getTypeVariables();

    protected abstract FunckyType bind(final Map<FunckyTypeVariable, FunckyType> bindings);
}
