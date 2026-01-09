package io.github.thanospapapetrou.funcky.compiler.linker;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.function.Predicate;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType.RECORD;

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
            return unify((FunckyType) fta.getDomain().eval(fta.getContext()),
                    (FunckyType) ftb.getDomain().eval(ftb.getContext()))
                    && unify((FunckyType) fta.getRange().eval(fta.getContext()),
                    (FunckyType) ftb.getRange().eval(ftb.getContext()));
        } else if ((ta instanceof FunckyListType lta) && (tb instanceof FunckyListType ltb)) {
            return unify((FunckyType) lta.getElement().eval(lta.getContext()),
                    (FunckyType) ltb.getElement().eval(ltb.getContext()));
        } else if ((ta instanceof FunckyRecordType rta) && (tb instanceof FunckyRecordType rtb)) {
            FunckyList rtaComponents = (FunckyList) rta.getComponents().eval(rta.getContext());
            FunckyList rtbComponents = (FunckyList) rtb.getComponents().eval(rtb.getContext());
            while ((rtaComponents.getHead() != null) && (rtbComponents.getHead() != null)) {
                final boolean unify = unify(
                        (FunckyType) rtaComponents.getHead().eval(rtaComponents.getContext()),
                        (FunckyType) rtbComponents.getHead().eval(rtbComponents.getContext()));
                if (!unify) {
                    return false;
                }
                rtaComponents = (FunckyList) rtaComponents.getTail().eval(rtaComponents.getContext());
                rtbComponents = (FunckyList) rtbComponents.getTail().eval(rtbComponents.getContext());
            }
            return ((rtaComponents.getHead() == null) && (rtbComponents.getHead() == null));
        } else if ((ta instanceof FunckyMonadicType mta) && (tb instanceof FunckyMonadicType mtb)) {
            return mta.getName().equals(mtb.getName()) && unify(
                    (FunckyType) mta.getBase().eval(mta.getContext()),
                    (FunckyType) mtb.getBase().eval(mtb.getContext()));
        } else if ((ta instanceof FunckyTypeVariable) || (tb instanceof FunckyTypeVariable)) {
            return union(ta, tb);
        }
        return false;
    }

    public FunckyType find(final FunckyType type) {
        if (type instanceof FunckySimpleType) {
            return type;
        } else if (type instanceof FunckyFunctionType ft) {
            return FUNCTION(
                    engine -> find((FunckyType) ft.getDomain().eval(ft.getContext())),
                    engine -> find((FunckyType) ft.getRange().eval(ft.getContext()))
            ).apply(ft.getContext());
        } else if (type instanceof FunckyListType lt) {
            return LIST(engine -> find((FunckyType) lt.getElement().eval(lt.getContext())))
                    .apply(lt.getContext());
        } else if (type instanceof FunckyRecordType rt) {
            return find(rt);
        } else if (type instanceof FunckyMonadicType mt) {
            return new FunckyMonadicType(mt.getContext(), mt.getName(),
                    new FunckyLiteral(find((FunckyType) mt.getBase().eval(mt.getContext()))));
        } else if (type instanceof FunckyTypeVariable) {
            final FunckyType found = findRepresentative(findSet(type));
            if (found instanceof FunckyFunctionType ft) {
                return FUNCTION(
                        engine -> find((FunckyType) ft.getDomain().eval(ft.getContext())),
                        engine -> find((FunckyType) ft.getRange().eval(ft.getContext()))
                ).apply(ft.getContext());
            } else if (found instanceof FunckyListType lt) {
                return LIST(engine -> find((FunckyType) lt.getElement().eval(lt.getContext())))
                        .apply(lt.getContext());
            } else if (found instanceof FunckyRecordType rt) {
                return find(rt);
            } else if (found instanceof FunckyMonadicType mt) {
                return new FunckyMonadicType(mt.getContext(), mt.getName(),
                        new FunckyLiteral(find((FunckyType) mt.getBase().eval(mt.getContext()))));
            }
            return found;
        }
        return null;
    }

    private FunckyRecordType find(final FunckyRecordType type) {
        final List<FunckyType> components = new ArrayList<>();
        for (FunckyList list = (FunckyList) type.getComponents().eval(type.getContext());
                list.getTail() != null; list = (FunckyList) list.getTail().eval(type.getContext())) {
            components.add(find((FunckyType) list.getHead().eval(type.getContext())));
        }
        return (FunckyRecordType) RECORD(components.stream()
                .map(t -> (Function<FunckyContext, FunckyType>) (c -> t))
                .toList().toArray(new Function[0])).apply(type.getContext());
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
