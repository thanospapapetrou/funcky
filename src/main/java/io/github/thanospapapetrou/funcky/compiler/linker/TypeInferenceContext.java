package io.github.thanospapapetrou.funcky.compiler.linker;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;

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
        } else if ((ta instanceof FunckyFunctionType) && (tb instanceof FunckyFunctionType)) {
            return unify((FunckyType) ((FunckyFunctionType) ta).getDomain().eval((ScriptContext) null),
                    (FunckyType) ((FunckyFunctionType) tb).getDomain().eval((ScriptContext) null)) && unify(
                    (FunckyType) ((FunckyFunctionType) ta).getRange().eval((ScriptContext) null),
                    (FunckyType) ((FunckyFunctionType) tb).getRange().eval((ScriptContext) null));
        } else if ((ta instanceof FunckyListType) && (tb instanceof FunckyListType)) {
            return unify((FunckyType) ((FunckyListType) ta).getElement().eval((ScriptContext) null),
                    (FunckyType) ((FunckyListType) tb).getElement().eval((ScriptContext) null));
        } else if ((ta instanceof FunckyRecordType) && (tb instanceof FunckyRecordType)) {
            FunckyList taComponents =
                    (FunckyList) ((FunckyRecordType) ta).getComponents().eval((ScriptContext) null);
            FunckyList tbComponents =
                    (FunckyList) ((FunckyRecordType) tb).getComponents().eval((ScriptContext) null);
            while ((taComponents.getHead() != null) && (tbComponents.getHead() != null)) {
                final boolean unify =
                        unify((FunckyType) taComponents.getHead().eval((ScriptContext) null),
                                (FunckyType) tbComponents.getHead().eval((ScriptContext) null));
                if (!unify) {
                    return false;
                }
                taComponents = (FunckyList) taComponents.getTail().eval((ScriptContext) null);
                tbComponents = (FunckyList) tbComponents.getTail().eval((ScriptContext) null);
            }
            return ((taComponents.getHead() == null) && (tbComponents.getHead() == null));
        } else if ((ta instanceof FunckyTypeVariable) || (tb instanceof FunckyTypeVariable)) {
            return union(ta, tb);
        }
        return false;
    }

    public FunckyType find(final FunckyType type) {
        if (type instanceof FunckySimpleType) {
            return type;
        } else if (type instanceof FunckyFunctionType) {
            return FunckyFunctionType.FUNCTION(
                    find((FunckyType) ((FunckyFunctionType) type).getDomain().eval((ScriptContext) null)),
                    find((FunckyType) ((FunckyFunctionType) type).getRange().eval((ScriptContext) null)));
        } else if (type instanceof FunckyListType) {
            return FunckyListType.LIST(
                    find((FunckyType) ((FunckyListType) type).getElement().eval((ScriptContext) null)));
        } else if (type instanceof FunckyRecordType) {
            final List<FunckyType> components = new ArrayList<>();
            for (FunckyList list =
                    (FunckyList) ((FunckyRecordType) type).getComponents().eval((ScriptContext) null);
                    list.getTail() != null; list = (FunckyList) list.getTail().eval((ScriptContext) null)) {
                components.add(find((FunckyType) list.getHead().eval((ScriptContext) null)));
            }
            return FunckyRecordType.RECORD(components.toArray(new FunckyType[0]));
        } else if (type instanceof FunckyTypeVariable) {
            final FunckyType found = findRepresentative(findSet(type));
            if (found instanceof FunckyFunctionType) {
                return new FunckyFunctionType(new FunckyLiteral(null,
                        find((FunckyType) ((FunckyFunctionType) found).getDomain()
                                .eval((ScriptContext) null))),
                        new FunckyLiteral(null, find((FunckyType) ((FunckyFunctionType) found).getRange()
                                .eval((ScriptContext) null))));
            } else if (found instanceof FunckyListType) {
                return FunckyListType.LIST(
                        find((FunckyType) ((FunckyListType) found).getElement().eval((ScriptContext) null)));
            } else if (found instanceof FunckyRecordType) {
                final List<FunckyType> components = new ArrayList<>();
                for (FunckyList list =
                        (FunckyList) ((FunckyRecordType) found).getComponents().eval((ScriptContext) null);
                        list.getTail() != null;
                        list = (FunckyList) list.getTail().eval((ScriptContext) null)) {
                    components.add(find((FunckyType) list.getHead().eval((ScriptContext) null)));
                }
                return FunckyRecordType.RECORD(components.toArray(new FunckyType[0]));
            }
            return found;
        }
        return null;
    }

    private boolean union(final FunckyType a, final FunckyType b) {
        final Set<FunckyType> setA = findSet(a);
        final Set<FunckyType> setB = findSet(b);
        if ((findRepresentative(setA) instanceof FunckyTypeVariable) || (findRepresentative(
                setB) instanceof FunckyTypeVariable)) {
            context.remove(setB);
            context.remove(setA);
            setA.addAll(setB);
            context.add(setA);
            return true;
        }
        return false;
    }

    private Set<FunckyType> findSet(final FunckyType type) {
        return context.stream().filter(ts -> ts.contains(type)).findFirst().orElseGet(() -> {
            final Set<FunckyType> types = new TreeSet<>();
            types.add(type);
            this.context.add(types);
            return types;
        });
    }

    private FunckyType findRepresentative(final Set<FunckyType> types) {
        return types.stream().filter(Predicate.not(FunckyTypeVariable.class::isInstance)).findFirst()
                .orElse(types.stream().findFirst().orElse(null));
    }
}
