package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;
import java.util.function.Function;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public final class Types extends FunckyLibrary {
    public static final Function<FunckyEngine, HigherOrderFunction> FUNCTION =
            engine -> new HigherOrderFunction(engine, Types.class, "Function", FunckySimpleType.TYPE.apply(engine),
                    FunckySimpleType.TYPE.apply(engine), FunckySimpleType.TYPE.apply(engine)) {
        @Override
        protected FunckyFunctionType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyFunctionType(engine, arguments.get(0), arguments.get(1));
        }
    };
    public static final Function<FunckyEngine, HigherOrderFunction> DOMAIN =
            engine -> new HigherOrderFunction(engine, Types.class, "domain", FunckySimpleType.TYPE.apply(engine),
                    FunckySimpleType.TYPE.apply(engine)) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyFunctionType) {
                return (FunckyType) ((FunckyFunctionType) type).getDomain().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_DOMAIN, type));
        }
    };
    public static final Function<FunckyEngine, HigherOrderFunction> RANGE =
            engine -> new HigherOrderFunction(engine, Types.class, "range", FunckySimpleType.TYPE.apply(engine),
                    FunckySimpleType.TYPE.apply(engine)) {
        @Override
        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
            if (type instanceof FunckyFunctionType) {
                return (FunckyType) ((FunckyFunctionType) type).getRange().eval(context);
            }
            throw new SneakyRuntimeException(String.format(ERROR_RANGE, type));
        }
    };
    public static final Function<FunckyEngine, HigherOrderFunction> LIST =
            engine -> new HigherOrderFunction(engine, Types.class, "List", FunckySimpleType.TYPE.apply(engine),
                    FunckySimpleType.TYPE.apply(engine)) {
        @Override
        protected FunckyListType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyListType(engine, arguments.getFirst());
        }
    };
    public static final Function<FunckyEngine, HigherOrderFunction> ELEMENT =
            engine -> new HigherOrderFunction(engine, Types.class, "element", FunckySimpleType.TYPE.apply(engine),
                    FunckySimpleType.TYPE.apply(engine)) {
                @Override
                protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
                    final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
                    if (type instanceof FunckyListType) {
                        return (FunckyType) ((FunckyListType) type).getElement().eval(context);
                    }
                    throw new SneakyRuntimeException(String.format(ERROR_ELEMENT, type));
                }
            };
    public static final Function<FunckyEngine, HigherOrderFunction> RECORD =
            engine -> new HigherOrderFunction(engine, Types.class, "Record",
                    new FunckyListType(engine, FunckySimpleType.TYPE.apply(engine)),
                    FunckySimpleType.TYPE.apply(engine)) {
        @Override
        protected FunckyRecordType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyRecordType(engine, arguments.getFirst());
        }
    };
    //    public static final Function<FunckyEngine, HigherOrderFunction> COMPONENTS =
    //            new HigherOrderFunction(Types.class, "components",
    //            FunckySimpleType.TYPE, new FunckyListType(FunckySimpleType.TYPE)) {
    //        @Override
    //        protected FunckyList apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            final FunckyType type = (FunckyType) arguments.getFirst().eval(context);
    //            if (type instanceof FunckyRecordType) {
    //                return (FunckyList) ((FunckyRecordType) type).getComponents().eval(context);
    //            }
    //            throw new SneakyRuntimeException(String.format(ERROR_COMPONENTS, type));
    //        }
    //    };
    //    public static final Function<FunckyEngine, HigherOrderFunction> TYPE = new HigherOrderFunction(Types.class, "type",
    //            new FunckyTypeVariable(), FunckySimpleType.TYPE) {
    //        @Override
    //        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //                return arguments.getFirst().getType();
    //        }
    //    };
    //    public static final Function<FunckyEngine, HigherOrderFunction> TYPE_VARIABLE =
    //            new HigherOrderFunction(Types.class, "typeVariable",
    //            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
    //        @Override
    //        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            return (arguments.getFirst().eval(context) instanceof FunckyTypeVariable) ? FunckyBoolean.TRUE
    //                    : FunckyBoolean.FALSE;
    //        }
    //    };
    //    public static final Function<FunckyEngine, HigherOrderFunction> FUNCTION_TYPE =
    //            new HigherOrderFunction(Types.class, "functionType",
    //            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
    //        @Override
    //        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            return (arguments.getFirst().eval(context) instanceof FunckyFunctionType) ? FunckyBoolean.TRUE
    //                    : FunckyBoolean.FALSE;
    //        }
    //    };
    //    public static final Function<FunckyEngine, HigherOrderFunction> LIST_TYPE =
    //            new HigherOrderFunction(Types.class, "listType",
    //            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
    //        @Override
    //        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            return (arguments.getFirst().eval(context) instanceof FunckyListType) ? FunckyBoolean.TRUE
    //                    : FunckyBoolean.FALSE;
    //        }
    //    };
    //    public static final Function<FunckyEngine, HigherOrderFunction> RECORD_TYPE =
    //            new HigherOrderFunction(Types.class, "recordType",
    //            FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN) {
    //        @Override
    //        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            return (arguments.getFirst().eval(context) instanceof FunckyRecordType) ? FunckyBoolean.TRUE
    //                    : FunckyBoolean.FALSE;
    //        }
    //    };
    //    public static final Function<FunckyEngine, HigherOrderFunction> FREE = new HigherOrderFunction(Types.class, "free",
    //            FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
    //        @Override
    //        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            return ((FunckyType) arguments.getFirst().eval(context)).free();
    //        }
    //    };
    //    public static final Function<FunckyEngine, HigherOrderFunction> UNIFY =
    //            new HigherOrderFunction(Types.class, "unify",
    //            FunckySimpleType.TYPE, FunckySimpleType.TYPE, FunckySimpleType.TYPE) {
    //        @Override
    //        protected FunckyType apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            final FunckyType type = (FunckyType) arguments.get(0).eval(context);
    //            final FunckyType otherType = (FunckyType) arguments.get(1).eval(context);
    //            final FunckyType result = type.unify(otherType);
    //            if (result == null) {
    //                throw new SneakyRuntimeException(String.format(ERROR_UNIFY, type, otherType));
    //            }
    //            return result;
    //        }
    //    };
    private static final String ERROR_COMPONENTS = "Can not get components of non-record type `%1$s`";
    private static final String ERROR_DOMAIN = "Can not get domain of non-function type `%1$s`";
    private static final String ERROR_ELEMENT = "Can not get element of non-list type `%1$s`";
    private static final String ERROR_RANGE = "Can not get range of non-function type `%1$s`";
    private static final String ERROR_UNIFY = "Can not unify `%1$s` with `%2$s`";

    public Types(final FunckyEngine engine) {
        super(engine, FunckySimpleType.TYPE.apply(engine), FunckySimpleType.NUMBER.apply(engine),
                FunckySimpleType.BOOLEAN.apply(engine), FunckySimpleType.CHARACTER.apply(engine),
                FUNCTION.apply(engine), DOMAIN.apply(engine), RANGE.apply(engine), LIST.apply(engine),
                ELEMENT.apply(engine), RECORD.apply(engine));
        //                , COMPONENTS, TYPE, TYPE_VARIABLE, FUNCTION_TYPE,
        //                LIST_TYPE, RECORD_TYPE, FREE, UNIFY);
    }
}
