package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

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

    public final FunckySimpleType $Type = FunckySimpleType.TYPE;
    public final FunckySimpleType $Number = FunckySimpleType.NUMBER;
    public final FunckySimpleType $Boolean = FunckySimpleType.BOOLEAN;
    public final FunckySimpleType $Character = FunckySimpleType.CHARACTER;
    public final HigherOrderFunction $Function =
            new HigherOrderFunction(this, FunckySimpleType.TYPE, FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyFunctionType apply(final List<FunckyExpression> arguments) {
            return new FunckyFunctionType(arguments.get(0), arguments.get(1));
        }
    };
    public final HigherOrderFunction $domain =
            new HigherOrderFunction(this, FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval();
            if (type instanceof FunckyFunctionType) {
                return (FunckyType) ((FunckyFunctionType) type).getDomain().eval();
            }
            throw new SneakyRuntimeException(String.format(ERROR_DOMAIN, type));
        }
    };
    public final HigherOrderFunction $range =
            new HigherOrderFunction(this, FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval();
            if (type instanceof FunckyFunctionType) {
                return (FunckyType) ((FunckyFunctionType) type).getRange().eval();
            }
            throw new SneakyRuntimeException(String.format(ERROR_RANGE, type));
        }
    };
    public final HigherOrderFunction $List =
            new HigherOrderFunction(this, FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyListType apply(final List<FunckyExpression> arguments) {
            return new FunckyListType(arguments.getFirst());
        }
    };
    public final HigherOrderFunction $element =
            new HigherOrderFunction(this, FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval();
            if (type instanceof FunckyListType) {
                return (FunckyType) ((FunckyListType) type).getElement().eval();
            }
            throw new SneakyRuntimeException(String.format(ERROR_ELEMENT, type));
        }
    };
    public final HigherOrderFunction $Record = new HigherOrderFunction(this,
            FunckyListType.LIST(FunckySimpleType.TYPE), FunckySimpleType.TYPE) {
        @Override
        protected FunckyRecordType apply(final List<FunckyExpression> arguments) {
            return new FunckyRecordType(arguments.getFirst());
        }
    };
    public final HigherOrderFunction $components = new HigherOrderFunction(this,
            FunckySimpleType.TYPE, FunckyListType.LIST(FunckySimpleType.TYPE)) {
        @Override
        protected FunckyList apply(final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval();
            if (type instanceof FunckyRecordType) {
                return (FunckyList) ((FunckyRecordType) type).getComponents().eval();
            }
            throw new SneakyRuntimeException(String.format(ERROR_COMPONENTS, type));
        }
    };
    public HigherOrderFunction $type = new HigherOrderFunction(this, new FunckyTypeVariable(), FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final List<FunckyExpression> arguments) {
            return arguments.getFirst().getType();
        }
    };
    public final HigherOrderFunction $typeVariable = new HigherOrderFunction(this,
            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final List<FunckyExpression> arguments) {
            return (arguments.getFirst().eval() instanceof FunckyTypeVariable) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE;
                }
    };
    public final HigherOrderFunction $functionType = new HigherOrderFunction(this,
            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final List<FunckyExpression> arguments) {
            return (arguments.getFirst().eval() instanceof FunckyFunctionType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE;
        }
    };
    public final HigherOrderFunction $listType = new HigherOrderFunction(this,
            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final List<FunckyExpression> arguments) {
            return (arguments.getFirst().eval() instanceof FunckyListType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE;
        }
    };
    public final HigherOrderFunction $recordType = new HigherOrderFunction(this,
            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final List<FunckyExpression> arguments) {
            return (arguments.getFirst().eval() instanceof FunckyRecordType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE;
        }
    };
    public final HigherOrderFunction $free =
            new HigherOrderFunction(this, FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final List<FunckyExpression> arguments) {
            return ((FunckyType) arguments.getFirst().eval()).free();
        }
    };
    public final HigherOrderFunction $unify =
            new HigherOrderFunction(this, FunckySimpleType.TYPE, FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.get(0).eval();
            final FunckyType otherType = (FunckyType) arguments.get(1).eval();
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
