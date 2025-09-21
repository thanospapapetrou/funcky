package io.github.thanospapapetrou.funcky.runtime;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import io.github.thanospapapetrou.funcky.FunckyJavaConverter;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckyRecordType extends FunckyType {
    public static final FunckyRecordType UNIT = new FunckyRecordType(new FunckyList(
            new FunckyListType(FunckySimpleType.TYPE), (FunckyValue) null, null));

    private final FunckyExpression components;

    public FunckyRecordType(final FunckyExpression components) {
        this.components = components;
    }

    public FunckyRecordType(final FunckyList components) {
        this(new FunckyLiteral(components));
    }

    public FunckyExpression getComponents() {
        return components;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(Types.RECORD.toExpression(), components);
    }

    @Override
    public int compareTo(final FunckyType type) {
            final int classComparison = super.compareTo(type);
            return (classComparison == 0) ? ((FunckyList) components.eval()).compareTo(
                    (FunckyList) ((FunckyRecordType) type).components.eval()) : classComparison;
    }

    @Override
    public boolean equals(final Object object) {
            return (object instanceof FunckyRecordType) && components.eval()
                    .equals(((FunckyRecordType) object).components.eval());
    }

    @Override
    public int hashCode() {
            return components.eval().hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        final Set<FunckyTypeVariable> typeVariables = new HashSet<>();
        for (FunckyList list = (FunckyList) components.eval(); list.getTail() != null;
                list = (FunckyList) list.getTail().eval()) {
            typeVariables.addAll(((FunckyType) list.getHead().eval()).getTypeVariables());
        }
        return typeVariables;
    }

    @Override
    protected FunckyRecordType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        final List<FunckyType> types = new ArrayList<>();
        for (FunckyList list = (FunckyList) components.eval(); list.getTail() != null;
                list = (FunckyList) list.getTail().eval()) {
            types.add(((FunckyType) list.getHead().eval()).bind(bindings));
        }
        return new FunckyRecordType(FunckyJavaConverter.convert(types));
    }
}
