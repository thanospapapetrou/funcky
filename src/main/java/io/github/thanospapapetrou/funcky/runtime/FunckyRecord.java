package io.github.thanospapapetrou.funcky.runtime;

import java.util.ArrayList;
import java.util.List;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyFunckyRuntimeException;

public final class FunckyRecord extends FunckyValue implements Comparable<FunckyRecord> {
    private static final String DELIMITER = ", ";
    private static final String PREFIX = "{";
    private static final String SUFFIX = "}";

    private final FunckyRecordType type;
    private final List<FunckyExpression> components;

    public FunckyRecord(final FunckyRecordType type, final List<FunckyExpression> components) {
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
        return new FunckyLiteral(this);
    }

    @Override
    public int compareTo(final FunckyRecord record) {
        try {
            for (int i = 0; i < components.size(); i++) {
                final int componentComparison =
                        ((Comparable<FunckyValue>) components.get(i).eval()).compareTo(record.components.get(i).eval());
                if (componentComparison != 0) {
                    return componentComparison;
                }
            }
            return 0;
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    public boolean equals(final Object object) {
        try {
            return (object instanceof FunckyRecord) && eval().equals(((FunckyRecord) object).eval());
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    public int hashCode() {
        try {
            int hashCode = 0;
            for (final FunckyExpression component : components) {
                hashCode += component.eval().hashCode();
            }
            return hashCode;
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    @Override
    public String toString() {
        try {
            final StringBuilder string = new StringBuilder(PREFIX);
            for (final FunckyExpression component : components) {
                string.append(component.eval().toString()).append(DELIMITER);
            }
            if (string.length() > PREFIX.length()) {
                string.setLength(string.length() - DELIMITER.length());
            }
            return string.append(SUFFIX).toString();
        } catch (final FunckyRuntimeException e) {
            throw new SneakyFunckyRuntimeException(e);
        }
    }

    private List<FunckyValue> eval() throws FunckyRuntimeException {
        final List<FunckyValue> values = new ArrayList<>();
        for (final FunckyExpression component : components) {
            values.add(component.eval());
        }
        return values;
    }
}
