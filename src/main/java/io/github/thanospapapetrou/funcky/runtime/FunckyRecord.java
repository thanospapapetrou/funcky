package io.github.thanospapapetrou.funcky.runtime;

import java.util.List;
import java.util.stream.Collectors;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType;

public final class FunckyRecord extends FunckyValue {
    public static final String DELIMITER = ", ";
    public static final String PREFIX = "{";
    public static final String SUFFIX = "}";

    private final FunckyRecordType type;
    private final List<FunckyExpression> components;

    public FunckyRecord(final FunckyEngine engine, final FunckyRecordType type,
            final List<FunckyExpression> components) {
        super(engine);
        this.type = type;
        this.components = components;
    }

    public List<FunckyExpression> getComponents() {
        return components;
    }

    @Override
    public FunckyRecordType getType() {
        return type;
    }

    @Override
    public FunckyLiteral toExpression() {
        return new FunckyLiteral(engine, this);
    }

    @Override
    public int compareTo(final FunckyValue value) {
        if (value instanceof FunckyRecord record) {
            for (int i = 0; (i < components.size()) && (i < record.components.size()); i++) {
                final int comparison = components.get(i).eval(engine.getContext())
                        .compareTo(record.components.get(i).eval(engine.getContext()));
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
                .map(component -> component.eval(engine.getContext()))
                .mapToInt(FunckyValue::hashCode)
                .sum();
    }

    @Override
    public String toString() {
        return PREFIX + components.stream()
                .map(component -> component.eval(engine.getContext()))
                .map(FunckyValue::toString)
                .collect(Collectors.joining(DELIMITER)) + SUFFIX;
    }
}
