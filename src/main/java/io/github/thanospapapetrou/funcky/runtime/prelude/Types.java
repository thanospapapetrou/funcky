package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.BOOLEAN;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.TYPE;

public final class Types extends FunckyLibrary {
    private static final String ERROR_BASE = "Can not get base of non-monad type `%1$s`";
    private static final String ERROR_COMPONENTS = "Can not get components of non-record type `%1$s`";
    private static final String ERROR_DOMAIN = "Can not get domain of non-function type `%1$s`";
    private static final String ERROR_ELEMENT = "Can not get element of non-list type `%1$s`";
    private static final String ERROR_RANGE = "Can not get range of non-function type `%1$s`";
    private static final String ERROR_UNIFY = "Can not unify `%1$s` with `%2$s`";

    public final FunckySimpleType Type = FunckySimpleType.TYPE.apply(engine);
    public final FunckySimpleType Number = FunckySimpleType.NUMBER.apply(engine);
    public final FunckySimpleType Boolean = FunckySimpleType.BOOLEAN.apply(engine);
    public final FunckySimpleType Character = FunckySimpleType.CHARACTER.apply(engine);
    public final HigherOrderFunction Function = new HigherOrderFunction(engine, TYPE, TYPE, TYPE) {
        @Override
        public FunckyFunctionType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyFunctionType(engine, arguments.get(0), arguments.get(1));
        }
    };
    public final HigherOrderFunction domain = new HigherOrderFunction(engine, TYPE, TYPE) {
        @Override
        public FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyFunctionType function) {
                return (FunckyType) function.getDomain().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_DOMAIN, type));
        }
    };
    public final HigherOrderFunction range = new HigherOrderFunction(engine, TYPE, TYPE) {
        @Override
        public FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyFunctionType function) {
                return (FunckyType) function.getRange().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_RANGE, type));
        }
    };
    public final HigherOrderFunction List = new HigherOrderFunction(engine, TYPE, TYPE) {
        @Override
        public FunckyListType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyListType(engine, arguments.getFirst());
        }
    };
    public final HigherOrderFunction element = new HigherOrderFunction(engine, TYPE, TYPE) {
        @Override
        public FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyListType list) {
                return (FunckyType) list.getElement().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_ELEMENT, type));
        }
    };
    public final HigherOrderFunction Record = new HigherOrderFunction(engine,
            LIST(TYPE), TYPE) {
        @Override
        public FunckyRecordType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyRecordType(engine, arguments.getFirst());
        }
    };
    public final HigherOrderFunction components = new HigherOrderFunction(engine, TYPE,
            LIST(TYPE)) {
        @Override
        public FunckyList apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyRecordType record) {
                return (FunckyList) record.getComponents().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_COMPONENTS, type));
        }
    };
    public final HigherOrderFunction Maybe = new HigherOrderFunction(engine, TYPE, TYPE) {
        @Override
        public FunckyMonadicType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return FunckyMonadicType.maybe(engine, arguments.getFirst());
        }
    };
    public final HigherOrderFunction IO = new HigherOrderFunction(engine, TYPE, TYPE) {
        @Override
        public FunckyMonadicType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return FunckyMonadicType.io(engine, arguments.getFirst());
        }
    };
    public final HigherOrderFunction base = new HigherOrderFunction(engine, TYPE, TYPE) {
        @Override
        public FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyMonadicType monad) {
                return (FunckyType) monad.getBase().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_BASE, type));
        }
    };
    public HigherOrderFunction type = new HigherOrderFunction(engine, FunckyTypeVariable::new, TYPE) {
        @Override
        public FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return arguments.getFirst().getType();
        }
    };
    public final HigherOrderFunction typeVariable = new HigherOrderFunction(engine, TYPE, BOOLEAN) {
        @Override
        public FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return ((arguments.getFirst().eval(context) instanceof FunckyTypeVariable) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(engine);
                }
    };
    public final HigherOrderFunction functionType = new HigherOrderFunction(engine, TYPE, BOOLEAN) {
        @Override
        public FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return ((arguments.getFirst().eval(context) instanceof FunckyFunctionType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(engine);
        }
    };
    public final HigherOrderFunction listType = new HigherOrderFunction(engine, TYPE, BOOLEAN) {
        @Override
        public FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return ((arguments.getFirst().eval(context) instanceof FunckyListType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(engine);
        }
    };
    public final HigherOrderFunction recordType = new HigherOrderFunction(engine, TYPE, BOOLEAN) {
        @Override
        public FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return ((arguments.getFirst().eval(context) instanceof FunckyRecordType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(engine);
        }
    };
    public final HigherOrderFunction free = new HigherOrderFunction(engine, TYPE, TYPE) {
        @Override
        public FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return ((FunckyType) arguments.getFirst().eval(context)).free();
        }
    };
    public final HigherOrderFunction unify = new HigherOrderFunction(engine, TYPE, TYPE, TYPE) {
        @Override
        public FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.get(0).eval(context);
            final FunckyType otherType = (FunckyType) arguments.get(1).eval(context);
            final FunckyType result = type.unify(otherType);
            if (result == null) {
                throw new SneakyRuntimeException(String.format(ERROR_UNIFY, type, otherType));
            }
            return result;
        }
    };

    public Types(final FunckyEngine engine) {
        super(engine);
    }
}
