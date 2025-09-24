package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public final class Types extends FunckyLibrary {
    private static final String ERROR_COMPONENTS = "Can not get components of non-record type `%1$s`";
    private static final String ERROR_DOMAIN = "Can not get domain of non-function type `%1$s`";
    private static final String ERROR_ELEMENT = "Can not get element of non-list type `%1$s`";
    private static final String ERROR_RANGE = "Can not get range of non-function type `%1$s`";
    private static final String ERROR_UNIFY = "Can not unify `%1$s` with `%2$s`";

    public final FunckySimpleType $Type = FunckySimpleType.TYPE.apply(engine);
    public final FunckySimpleType $Number = FunckySimpleType.NUMBER.apply(engine);
    public final FunckySimpleType $Boolean = FunckySimpleType.BOOLEAN.apply(engine);
    public final FunckySimpleType $Character = FunckySimpleType.CHARACTER.apply(engine);
    public final HigherOrderFunction $Function = new HigherOrderFunction(engine, this, $Type, $Type, $Type) {
        @Override
        protected FunckyFunctionType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyFunctionType(engine, arguments.get(0), arguments.get(1));
        }
    };
    public final HigherOrderFunction $domain = new HigherOrderFunction(engine, this, $Type, $Type) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyFunctionType) {
                return (FunckyType) ((FunckyFunctionType) type).getDomain().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_DOMAIN, type));
        }
    };
    public final HigherOrderFunction $range = new HigherOrderFunction(engine, this, $Type, $Type) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyFunctionType) {
                return (FunckyType) ((FunckyFunctionType) type).getRange().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_RANGE, type));
        }
    };
    public final HigherOrderFunction $List = new HigherOrderFunction(engine, this, $Type, $Type) {
        @Override
        protected FunckyListType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyListType(engine, arguments.getFirst());
        }
    };
    public final HigherOrderFunction $element = new HigherOrderFunction(engine, this, $Type, $Type) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyListType) {
                return (FunckyType) ((FunckyListType) type).getElement().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_ELEMENT, type));
        }
    };
    public final HigherOrderFunction $Record = new HigherOrderFunction(engine, this,
            new FunckyListType(engine, $Type), $Type) {
        @Override
        protected FunckyRecordType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyRecordType(engine, arguments.getFirst());
        }
    };
    public final HigherOrderFunction $components = new HigherOrderFunction(engine, this, $Type,
            new FunckyListType(engine, $Type)) {
        @Override
        protected FunckyList apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyRecordType) {
                return (FunckyList) ((FunckyRecordType) type).getComponents().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_COMPONENTS, type));
        }
    };
    public HigherOrderFunction $type = new HigherOrderFunction(engine, this,
            new FunckyTypeVariable(engine), $Type) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return arguments.getFirst().getType();
        }
    };
    public final HigherOrderFunction $typeVariable = new HigherOrderFunction(engine, this, $Type, $Boolean) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return ((arguments.getFirst().eval(context) instanceof FunckyTypeVariable) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(engine);
                }
    };
    public final HigherOrderFunction $functionType = new HigherOrderFunction(engine, this, $Type, $Boolean) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return ((arguments.getFirst().eval(context) instanceof FunckyFunctionType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(engine);
        }
    };
    public final HigherOrderFunction $listType = new HigherOrderFunction(engine, this, $Type, $Boolean) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return ((arguments.getFirst().eval(context) instanceof FunckyListType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(engine);
        }
    };
    public final HigherOrderFunction $recordType = new HigherOrderFunction(engine, this, $Type, $Boolean) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return ((arguments.getFirst().eval(context) instanceof FunckyRecordType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(engine);
        }
    };
    public final HigherOrderFunction $free = new HigherOrderFunction(engine, this, $Type, $Type) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return ((FunckyType) arguments.getFirst().eval(context)).free();
        }
    };
    public final HigherOrderFunction $unify = new HigherOrderFunction(engine, this, $Type, $Type, $Type) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
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
