package io.github.thanospapapetrou.funcky.runtime;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public final class FunckyRecordType extends FunckyType {
    public static final Function<FunckyEngine, FunckyRecordType> UNIT = engine -> new FunckyRecordType(engine,
            new FunckyList(engine, new FunckyListType(engine, FunckySimpleType.TYPE.apply(engine)), (FunckyValue) null,
                    null));

    private final FunckyExpression components;

    public FunckyRecordType(final FunckyEngine engine, final FunckyExpression components) {
        super(engine);
        this.components = components;
    }

    public FunckyRecordType(final FunckyEngine engine, final FunckyList components) {
        this(engine, new FunckyLiteral(engine, components));
    }

    public FunckyExpression getComponents() {
        return components;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(new Types(engine).$Record.toExpression(), components);
    }

    @Override
    public int compareTo(final FunckyType type) {
            final int classComparison = super.compareTo(type);
        return (classComparison == 0) ? ((FunckyList) components.eval(engine.getContext())).compareTo(
                (FunckyList) ((FunckyRecordType) type).components.eval(engine.getContext())) : classComparison;
    }

    @Override
    public boolean equals(final Object object) {
        return (object instanceof FunckyRecordType) && components.eval(engine.getContext())
                .equals(((FunckyRecordType) object).components.eval(engine.getContext()));
    }

    @Override
    public int hashCode() {
        return components.eval(engine.getContext()).hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        final Set<FunckyTypeVariable> typeVariables = new HashSet<>();
        for (FunckyList list = (FunckyList) components.eval(engine.getContext()); list.getTail() != null;
                list = (FunckyList) list.getTail().eval(engine.getContext())) {
            typeVariables.addAll(((FunckyType) list.getHead().eval(engine.getContext())).getTypeVariables());
        }
        return typeVariables;
    }

    @Override
    protected FunckyRecordType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        final List<FunckyType> types = new ArrayList<>();
        for (FunckyList list = (FunckyList) components.eval(engine.getContext()); list.getTail() != null;
                list = (FunckyList) list.getTail().eval(engine.getContext())) {
            types.add(((FunckyType) list.getHead().eval(engine.getContext())).bind(bindings));
        }
        return new FunckyRecordType(engine, engine.getConverter().convert(types));
    }
}
