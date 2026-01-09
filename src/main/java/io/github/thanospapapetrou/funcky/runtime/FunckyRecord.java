package io.github.thanospapapetrou.funcky.runtime;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public final class FunckyRecord extends FunckyValue {
    public static final String DELIMITER = ", ";
    public static final String PREFIX = "{";
    public static final String SUFFIX = "}";

    private final List<FunckyExpression> components;

    public FunckyRecord(final FunckyContext context, final List<FunckyExpression> components) {
        super(context);
        this.components = components;
    }

    public FunckyRecord(final FunckyContext context) {
        this(context, List.of());
    }

    public List<FunckyExpression> getComponents() {
        return components;
    }

    @Override
    public FunckyRecordType getType() {
        return (FunckyRecordType) FunckyRecordType.RECORD(components.stream()
                .map(FunckyExpression::getType)
                .map(t -> (Function<FunckyContext, FunckyType>) (c -> t))
                .toList().toArray(new Function[0])).apply(context);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        if (value instanceof FunckyRecord record) {
            for (int i = 0; (i < components.size()) && (i < record.components.size()); i++) {
                final int comparison = components.get(i).eval(context)
                        .compareTo(record.components.get(i).eval(context));
                if (comparison != 0) {
                    return comparison;
                }
            }
            return Integer.compare(components.size(), record.components.size());
        }
        return super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return components.stream()
                .map(component -> component.eval(context))
                .mapToInt(FunckyValue::hashCode)
                .sum();
    }

    @Override
    public String toString() {
        return PREFIX + components.stream()
                .map(component -> component.eval(context))
                .map(FunckyValue::toString)
                .collect(Collectors.joining(DELIMITER)) + SUFFIX;
    }
}
