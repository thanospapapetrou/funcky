package io.github.thanospapapetrou.funcky.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckyRecordType extends FunckyType {
    public static final FunckyRecordType UNIT = RECORD();

    private final FunckyExpression components;

    public static FunckyRecordType RECORD(final FunckyType... components) {
        return new FunckyRecordType(new FunckyLiteral(null,
                new FunckyList(FunckyListType.LIST(FunckySimpleType.TYPE), (components.length > 0)
                ? new FunckyLiteral(null, components[0]) : null, (components.length > 0)
                ? RECORD(Arrays.copyOfRange(components, 1, components.length)).components : null)));
    }

    public FunckyRecordType(final FunckyExpression components) {
        this.components = components;
    }

    public FunckyExpression getComponents() {
        return components;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(new Types(null).$Record.toExpression(), components);
    }

    @Override
    public int compareTo(final FunckyType type) {
            final int classComparison = super.compareTo(type);
        return (classComparison == 0) ? ((FunckyList) components.eval((ScriptContext) null))
                .compareTo((FunckyList) ((FunckyRecordType) type).components.eval((ScriptContext) null))
                : classComparison;
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyRecordType) && components.eval((ScriptContext) null)
                .equals(((FunckyRecordType) object).components.eval((ScriptContext) null));
    }

    @Override
    public int hashCode() {
        return components.eval((ScriptContext) null).hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        final Set<FunckyTypeVariable> typeVariables = new HashSet<>();
        for (FunckyList list = (FunckyList) components.eval((ScriptContext) null); list.getTail() != null;
                list = (FunckyList) list.getTail().eval((ScriptContext) null)) {
            typeVariables.addAll(((FunckyType) list.getHead().eval((ScriptContext) null)).getTypeVariables());
        }
        return typeVariables;
    }

    @Override
    protected FunckyRecordType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        final List<FunckyType> types = new ArrayList<>();
        for (FunckyList list = (FunckyList) components.eval((ScriptContext) null); list.getTail() != null;
                list = (FunckyList) list.getTail().eval((ScriptContext) null)) {
            types.add(((FunckyType) list.getHead().eval((ScriptContext) null)).bind(bindings));
        }
        return RECORD(types.toArray(new FunckyType[0]));
    }
}
