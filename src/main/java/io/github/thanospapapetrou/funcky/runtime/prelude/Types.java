package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;

public final class Types extends FunckyLibrary {
    public static final HigherOrderFunction FUNCTION = new HigherOrderFunction(Types.class, "Function",
            FunckySimpleType.TYPE, FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyFunctionType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyFunctionType(arguments.get(0), arguments.get(1));
        }
    };
    public static final HigherOrderFunction DOMAIN = new HigherOrderFunction(Types.class, "domain",
            FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyFunctionType) {
                return (FunckyType) ((FunckyFunctionType) type).getDomain().eval(context);
            }
            throw new FunckyRuntimeException(String.format(ERROR_DOMAIN, type));
        }
    };
    public static final HigherOrderFunction RANGE = new HigherOrderFunction(Types.class, "range",
            FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyFunctionType) {
                return (FunckyType) ((FunckyFunctionType) type).getRange().eval(context);
            }
            throw new FunckyRuntimeException(String.format(ERROR_RANGE, type));
        }
    };
    public static final HigherOrderFunction LIST = new HigherOrderFunction(Types.class, "List",
            FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyListType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyListType(arguments.getFirst());
        }
    };
    public static final HigherOrderFunction ELEMENT = new HigherOrderFunction(Types.class, "element",
            FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyListType) {
                return (FunckyType) ((FunckyListType) type).getElement().eval(context);
            }
            throw new FunckyRuntimeException(String.format(ERROR_ELEMENT, type));
        }
    };
    public static final HigherOrderFunction RECORD = new HigherOrderFunction(Types.class, "Record",
            new FunckyListType(FunckySimpleType.TYPE), FunckySimpleType.TYPE) {
        @Override
        protected FunckyRecordType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyRecordType(arguments.getFirst());
        }
    };
    public static final HigherOrderFunction COMPONENTS = new HigherOrderFunction(Types.class, "components",
            FunckySimpleType.TYPE, new FunckyListType(FunckySimpleType.TYPE)) {
        @Override
        protected FunckyList apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyRecordType) {
                return (FunckyList) ((FunckyRecordType) type).getComponents().eval(context);
            }
            throw new FunckyRuntimeException(String.format(ERROR_COMPONENTS, type));
        }
    };
    public static final HigherOrderFunction TYPE = new HigherOrderFunction(Types.class, "type",
            new FunckyTypeVariable(), FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            try {
                return arguments.getFirst().getType();
            } catch (final CompilationException e) {
                throw new FunckyRuntimeException(e);
            }
        }
    };
    public static final HigherOrderFunction TYPE_VARIABLE = new HigherOrderFunction(Types.class, "typeVariable",
            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            return (arguments.getFirst().eval(context) instanceof FunckyTypeVariable) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE;
        }
    };
    public static final HigherOrderFunction FUNCTION_TYPE = new HigherOrderFunction(Types.class, "functionType",
            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            return (arguments.getFirst().eval(context) instanceof FunckyFunctionType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE;
        }
    };
    public static final HigherOrderFunction LIST_TYPE = new HigherOrderFunction(Types.class, "listType",
            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            return (arguments.getFirst().eval(context) instanceof FunckyListType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE;
        }
    };
    public static final HigherOrderFunction RECORD_TYPE = new HigherOrderFunction(Types.class, "recordType",
            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            return (arguments.getFirst().eval(context) instanceof FunckyRecordType) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE;
        }
    };
    public static final HigherOrderFunction FREE = new HigherOrderFunction(Types.class, "free",
            FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            return ((FunckyType) arguments.getFirst().eval(context)).free();
        }
    };
    public static final HigherOrderFunction UNIFY = new HigherOrderFunction(Types.class, "unify",
            FunckySimpleType.TYPE, FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            final FunckyType type = (FunckyType) arguments.get(0).eval(context);
            final FunckyType otherType = (FunckyType) arguments.get(1).eval(context);
            final FunckyType result = type.unify(otherType);
            if (result == null) {
                throw new FunckyRuntimeException(String.format(ERROR_UNIFY, type, otherType));
            }
            return result;
        }
    };
    private static final String ERROR_COMPONENTS = "Can not get components of non-record type `%1$s`";
    private static final String ERROR_DOMAIN = "Can not get domain of non-function type `%1$s`";
    private static final String ERROR_ELEMENT = "Can not get element of non-list type `%1$s`";
    private static final String ERROR_RANGE = "Can not get range of non-function type `%1$s`";
    private static final String ERROR_UNIFY = "Can not unify `%1$s` with `%2$s`";

    public Types() {
        super(FunckySimpleType.TYPE, FunckySimpleType.NUMBER, FunckySimpleType.BOOLEAN, FunckySimpleType.CHARACTER,
                FUNCTION, DOMAIN, RANGE, LIST, ELEMENT, RECORD, COMPONENTS, TYPE, TYPE_VARIABLE, FUNCTION_TYPE,
                LIST_TYPE, RECORD_TYPE, FREE, UNIFY);
    }
}
