package io.github.thanospapapetrou.funcky.runtime;

import java.util.ArrayList;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;

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
        return new FunckyLiteral(null, this);
    }

    @Override
    public int compareTo(final FunckyRecord record) {
            for (int i = 0; i < components.size(); i++) {
                final int componentComparison =
                        ((Comparable<FunckyValue>) components.get(i).eval((ScriptContext) null)).compareTo(
                                record.components.get(i).eval((ScriptContext) null));
                if (componentComparison != 0) {
                    return componentComparison;
                }
            }
            return 0;
    }

    @Override
    public boolean equals(final Object object) {
            return (object instanceof FunckyRecord) && eval().equals(((FunckyRecord) object).eval());
    }

    @Override
    public int hashCode() {
            int hashCode = 0;
            for (final FunckyExpression component : components) {
                hashCode += component.eval((ScriptContext) null).hashCode();
            }
            return hashCode;
    }

    @Override
    public String toString() {
            final StringBuilder string = new StringBuilder(PREFIX);
            for (final FunckyExpression component : components) {
                string.append(component.eval((ScriptContext) null).toString()).append(DELIMITER);
            }
            if (string.length() > PREFIX.length()) {
                string.setLength(string.length() - DELIMITER.length());
            }
            return string.append(SUFFIX).toString();
    }

    private List<FunckyValue> eval() {
        final List<FunckyValue> values = new ArrayList<>();
        for (final FunckyExpression component : components) {
            values.add(component.eval((ScriptContext) null));
        }
        return values;
    }
}
