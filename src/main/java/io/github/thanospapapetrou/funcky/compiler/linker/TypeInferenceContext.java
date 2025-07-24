package io.github.thanospapapetrou.funcky.compiler.linker;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;

import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public class TypeInferenceContext {
    private final Set<Set<FunckyType>> context;

    public TypeInferenceContext() {
        this(new HashSet<>());
    }

    private TypeInferenceContext(final Set<Set<FunckyType>> context) {
        this.context = context;
    }

    public boolean unify(final FunckyType a, final FunckyType b) throws FunckyRuntimeException {
        final FunckyType ta = find(a);
        final FunckyType tb = find(b);
        if ((ta instanceof FunckySimpleType) && (tb instanceof FunckySimpleType) && ta.equals(tb)) {
            return true;
        } else if ((ta instanceof FunckyFunctionType) && (tb instanceof FunckyFunctionType)) {
            return unify((FunckyType) ((FunckyFunctionType) ta).getDomain().eval(),
                    (FunckyType) ((FunckyFunctionType) tb).getDomain().eval())
                    && unify((FunckyType) ((FunckyFunctionType) ta).getRange().eval(),
                    (FunckyType) ((FunckyFunctionType) tb).getRange().eval());
        } else if ((ta instanceof FunckyListType) && (tb instanceof FunckyListType)) {
            return unify((FunckyType) ((FunckyListType) ta).getElement().eval(),
                    (FunckyType) ((FunckyListType) tb).getElement().eval());
        } else if ((ta instanceof FunckyRecordType) && (tb instanceof FunckyRecordType)) {
            FunckyList taComponents = (FunckyList) ((FunckyRecordType) ta).getComponents().eval();
            FunckyList tbComponents = (FunckyList) ((FunckyRecordType) tb).getComponents().eval();
            while ((taComponents.getHead() != null) && (tbComponents.getHead() != null)) {
                final boolean unify =
                        unify((FunckyType) taComponents.getHead().eval(), (FunckyType) tbComponents.getHead().eval());
                if (!unify) {
                    return false;
                }
                taComponents = (FunckyList) taComponents.getTail().eval();
                tbComponents = (FunckyList) tbComponents.getTail().eval();
            }
            return ((taComponents.getHead() == null) && (tbComponents.getHead() == null));
        } else if ((ta instanceof FunckyTypeVariable) || (tb instanceof FunckyTypeVariable)) {
            return union(ta, tb);
        }
        return false;
    }

    public FunckyType find(final FunckyType type) throws FunckyRuntimeException {
        if (type instanceof FunckySimpleType) {
            return type;
        } else if (type instanceof FunckyFunctionType) {
            return new FunckyFunctionType(find((FunckyType) ((FunckyFunctionType) type).getDomain().eval()),
                    find((FunckyType) ((FunckyFunctionType) type).getRange().eval()));
        } else if (type instanceof FunckyListType) {
            return new FunckyListType(find((FunckyType) ((FunckyListType) type).getElement().eval()));
        } else if (type instanceof FunckyRecordType) {
            final List<FunckyType> components = new ArrayList<>();
            for (FunckyList list = (FunckyList) ((FunckyRecordType) type).getComponents().eval();
                    list.getTail() != null; list = (FunckyList) list.getTail().eval()) {
                components.add(find((FunckyType) list.getHead().eval()));
            }
            return new FunckyRecordType(
                    ((FunckyRecordType) type).getComponents().getEngine().getConverter().convert(components));
        } else if (type instanceof FunckyTypeVariable) {
            final FunckyType found = findRepresentative(findSet(type));
            if (found instanceof FunckyFunctionType) {
                return new FunckyFunctionType(find((FunckyType) ((FunckyFunctionType) found).getDomain().eval()),
                        find((FunckyType) ((FunckyFunctionType) found).getRange().eval()));
            } else if (found instanceof FunckyListType) {
                return new FunckyListType(find((FunckyType) ((FunckyListType) found).getElement().eval()));
            } else if (found instanceof FunckyRecordType) {
                final List<FunckyType> components = new ArrayList<>();
                for (FunckyList list = (FunckyList) ((FunckyRecordType) found).getComponents().eval();
                        list.getTail() != null; list = (FunckyList) list.getTail().eval()) {
                    components.add(find((FunckyType) list.getHead().eval()));
                }
                return new FunckyRecordType(
                        ((FunckyRecordType) found).getComponents().getEngine().getConverter().convert(components));
            }
            return found;
        }
        return null;
    }

    private boolean union(final FunckyType a, final FunckyType b) {
        final Set<FunckyType> setA = findSet(a);
        final Set<FunckyType> setB = findSet(b);
        if ((findRepresentative(setA) instanceof FunckyTypeVariable)
                || (findRepresentative(setB) instanceof FunckyTypeVariable)) {
            context.remove(setB);
            context.remove(setA);
            setA.addAll(setB);
            context.add(setA);
            return true;
        }
        return false;
    }

    private Set<FunckyType> findSet(final FunckyType type) {
        return context.stream()
                .filter(ts -> ts.contains(type))
                .findFirst()
                .orElseGet(() -> {
                    final Set<FunckyType> types = new TreeSet<>();
                    types.add(type);
                    this.context.add(types);
                    return types;
                });
    }

    private FunckyType findRepresentative(final Set<FunckyType> types) {
        return types.stream()
                .filter(Predicate.not(FunckyTypeVariable.class::isInstance))
                .findFirst()
                .orElse(types.stream()
                        .findFirst()
                        .orElse(null));
    }
}
