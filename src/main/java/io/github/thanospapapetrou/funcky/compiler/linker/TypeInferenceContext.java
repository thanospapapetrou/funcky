package io.github.thanospapapetrou.funcky.compiler.linker;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.function.Predicate;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType;
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

    public boolean unify(final FunckyType a, final FunckyType b) {
        final FunckyType ta = find(a);
        final FunckyType tb = find(b);
        if ((ta instanceof FunckySimpleType) && (tb instanceof FunckySimpleType) && ta.equals(tb)) {
            return true;
        } else if ((ta instanceof FunckyFunctionType fta) && (tb instanceof FunckyFunctionType ftb)) {
            return unify((FunckyType) fta.getDomain().eval(ta.getEngine().getContext()),
                    (FunckyType) ftb.getDomain().eval(tb.getEngine().getContext()))
                    && unify((FunckyType) fta.getRange().eval(ta.getEngine().getContext()),
                    (FunckyType) ftb.getRange().eval(tb.getEngine().getContext()));
        } else if ((ta instanceof FunckyListType lta) && (tb instanceof FunckyListType ltb)) {
            return unify((FunckyType) lta.getElement().eval(ta.getEngine().getContext()),
                    (FunckyType) ltb.getElement().eval(tb.getEngine().getContext()));
        } else if ((ta instanceof FunckyRecordType rta) && (tb instanceof FunckyRecordType rtb)) {
            FunckyList taComponents = (FunckyList) rta.getComponents().eval(ta.getEngine().getContext());
            FunckyList tbComponents = (FunckyList) rtb.getComponents().eval(tb.getEngine().getContext());
            while ((taComponents.getHead() != null) && (tbComponents.getHead() != null)) {
                final boolean unify = unify(
                        (FunckyType) taComponents.getHead().eval(taComponents.getEngine().getContext()),
                        (FunckyType) tbComponents.getHead().eval(tbComponents.getEngine().getContext()));
                if (!unify) {
                    return false;
                }
                taComponents = (FunckyList) taComponents.getTail().eval(taComponents.getEngine().getContext());
                tbComponents = (FunckyList) tbComponents.getTail().eval(tbComponents.getEngine().getContext());
            }
            return ((taComponents.getHead() == null) && (tbComponents.getHead() == null));
        } else if ((ta instanceof FunckyMonadicType mta) && (tb instanceof FunckyMonadicType mtb)) {
            return mta.getName().equals(mtb.getName()) && unify(
                    (FunckyType) mta.getBase().eval(ta.getEngine().getContext()),
                    (FunckyType) mtb.getBase().eval(tb.getEngine().getContext()));
        } else if ((ta instanceof FunckyTypeVariable) || (tb instanceof FunckyTypeVariable)) {
            return union(ta, tb);
        }
        return false;
    }

    public FunckyType find(final FunckyType type) {
        if (type instanceof FunckySimpleType) {
            return type;
        } else if (type instanceof FunckyFunctionType ft) {
            return new FunckyFunctionType(type.getEngine(), new FunckyLiteral(type.getEngine(),
                    find((FunckyType) ft.getDomain().eval(type.getEngine().getContext()))),
                    new FunckyLiteral(type.getEngine(),
                            find((FunckyType) ft.getRange().eval(type.getEngine().getContext()))));
        } else if (type instanceof FunckyListType lt) {
            return new FunckyListType(type.getEngine(), new FunckyLiteral(type.getEngine(),
                    find((FunckyType) lt.getElement().eval(type.getEngine().getContext()))));
        } else if (type instanceof FunckyRecordType rt) {
            return find(rt);
        } else if (type instanceof FunckyMonadicType mt) {
            return new FunckyMonadicType(type.getEngine(), mt.getName(), new FunckyLiteral(type.getEngine(),
                    find((FunckyType) mt.getBase().eval(type.getEngine().getContext()))));
        } else if (type instanceof FunckyTypeVariable) {
            final FunckyType found = findRepresentative(findSet(type));
            if (found instanceof FunckyFunctionType ft) {
                return new FunckyFunctionType(type.getEngine(), new FunckyLiteral(type.getEngine(),
                        find((FunckyType) ft.getDomain().eval(found.getEngine().getContext()))),
                        new FunckyLiteral(type.getEngine(), find((FunckyType) ft.getRange()
                                .eval(found.getEngine().getContext()))));
            } else if (found instanceof FunckyListType lt) {
                return new FunckyListType(type.getEngine(), new FunckyLiteral(type.getEngine(),
                        find((FunckyType) lt.getElement().eval(found.getEngine().getContext()))));
            } else if (found instanceof FunckyRecordType rt) {
                return find(rt);
            } else if (found instanceof FunckyMonadicType mt) {
                return new FunckyMonadicType(type.getEngine(), mt.getName(), new FunckyLiteral(type.getEngine(),
                        find((FunckyType) mt.getBase().eval(found.getEngine().getContext()))));
            }
            return found;
        }
        return null;
    }

    private FunckyRecordType find(final FunckyRecordType type) {
        final List<FunckyType> components = new ArrayList<>();
        for (FunckyList list = (FunckyList) type.getComponents().eval(type.getEngine().getContext());
                list.getTail() != null; list = (FunckyList) list.getTail().eval(list.getEngine().getContext())) {
            components.add(find((FunckyType) list.getHead().eval(list.getEngine().getContext())));
        }
        return (FunckyRecordType) FunckyRecordType.RECORD(components.stream()
                .map(t -> (Function<FunckyEngine, FunckyType>) (e -> t))
                .toList().toArray(new Function[0])).apply(type.getEngine());
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
