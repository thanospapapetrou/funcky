package io.github.thanospapapetrou.funcky.runtime.types;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.TYPE;

public final class FunckyRecordType extends FunckyType {
    public static final Function<FunckyContext, FunckyRecordType> UNIT = RECORD();

    private final FunckyExpression components;

    public static Function<FunckyContext, FunckyRecordType> RECORD(
            final Function<FunckyContext, ? extends FunckyType>... components) {
        return context -> new FunckyRecordType(context, new FunckyLiteral(new FunckyList(context,
                LIST(TYPE).apply(context),
                (components.length > 0) ? new FunckyLiteral(components[0].apply(context)) : null,
                (components.length > 0) ? RECORD(Arrays.copyOfRange(components,
                1, components.length)).apply(context).components : null)));
    }

    public FunckyRecordType(final FunckyContext context, final FunckyExpression components) {
        super(context);
        this.components = components;
    }

    public FunckyExpression getComponents() {
        return components;
    }

    @Override
    public FunckyApplication toExpression() {
        return new FunckyApplication(
                new FunckyReference(context.getEngine(), FunckyLibrary.getNamespace(Types.class), "Record"),
                components); // TODO
    }

    @Override
    public int compareTo(final FunckyValue value) {
        return (value instanceof FunckyRecordType type) ? components.eval(context)
                .compareTo(type.components.eval(context)) : super.compareTo(value);
    }

    @Override
    public int hashCode() {
        return components.eval(context).hashCode();
    }

    @Override
    protected Set<FunckyTypeVariable> getTypeVariables() {
        final Set<FunckyTypeVariable> typeVariables = new HashSet<>();
        for (FunckyList list = (FunckyList) components.eval(context); list.getTail() != null;
                list = (FunckyList) list.getTail().eval(context)) {
            typeVariables.addAll(((FunckyType) list.getHead().eval(context)).getTypeVariables());
        }
        return typeVariables;
    }

    @Override
    protected FunckyRecordType bind(final Map<FunckyTypeVariable, FunckyType> bindings) {
        final List<FunckyType> types = new ArrayList<>();
        for (FunckyList list = (FunckyList) components.eval(context); list.getTail() != null;
                list = (FunckyList) list.getTail().eval(context)) {
            types.add(((FunckyType) list.getHead().eval(context)).bind(bindings));
        }
        return (FunckyRecordType) RECORD(types.stream()
                .map(t -> (Function<FunckyContext, FunckyType>) (c -> t))
                .toList().toArray(new Function[0])).apply(context);
    }
}
