package io.github.thanospapapetrou.funcky.runtime.types;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import io.github.thanospapapetrou.funcky.compiler.linker.TypeInferenceContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;

public abstract class FunckyType extends FunckyValue implements Comparable<FunckyType> {
    private static final List<Class<? extends FunckyType>> ORDERING = List.of(
            FunckySimpleType.class,
            FunckyFunctionType.class,
            FunckyListType.class,
            FunckyRecordType.class,
            FunckyTypeVariable.class);

    private static int getOrder(final FunckyType type) {
        return IntStream.range(0, ORDERING.size())
                .filter(i -> ORDERING.get(i).isInstance(type))
                .findFirst()
                .orElse(-1);
    }

    public FunckyType free() throws FunckyRuntimeException {
        return bind(getTypeVariables().stream()
                .collect(Collectors.toMap(Function.identity(), typeVariable -> new FunckyTypeVariable())));
    }

    public FunckyType unify(final FunckyType type) throws FunckyRuntimeException {
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
        return FunckySimpleType.TYPE;
    }

    @Override
    public int compareTo(final FunckyType type) {
        return Integer.compare(getOrder(this), getOrder(type));
    }

    protected abstract Set<FunckyTypeVariable> getTypeVariables() throws FunckyRuntimeException;

    protected abstract FunckyType bind(final Map<FunckyTypeVariable, FunckyType> bindings)
            throws FunckyRuntimeException;
}
