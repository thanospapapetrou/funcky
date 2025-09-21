package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyJavaConverter;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public final class Commons extends FunckyLibrary {
//    private static final FunckyTypeVariable A = new FunckyTypeVariable();
//    private static final FunckyTypeVariable B = new FunckyTypeVariable();
//    public static final HigherOrderFunction EQUAL = new HigherOrderFunction(Commons.class, "equal",
//            A, A, FunckySimpleType.BOOLEAN) {
//        @Override
//        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
//                return arguments.get(0).eval(context).equals(arguments.get(1).eval(context))
//                        ? FunckyBoolean.TRUE : FunckyBoolean.FALSE;
//        }
//    };
//    public static final HigherOrderFunction COMPARE = new HigherOrderFunction(Commons.class, "compare",
//            A, A, FunckySimpleType.NUMBER) {
//        @Override
//        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
//                return new FunckyNumber(new BigDecimal(
//                        Integer.compare(((Comparable<FunckyValue>) arguments.get(0).eval(context))
//                                .compareTo(arguments.get(1).eval(context)), 0)));
//        }
//    };
//    public static final HigherOrderFunction HASH = new HigherOrderFunction(Commons.class, "hash",
//            A, FunckySimpleType.NUMBER) {
//        @Override
//        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
//                return new FunckyNumber(new BigDecimal(arguments.getFirst().eval(context).hashCode()));
//        }
//    };
//    public static final HigherOrderFunction IF = new HigherOrderFunction(Commons.class, "if",
//            FunckySimpleType.BOOLEAN, A, A, A) {
//        @Override
//        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
//            return (((FunckyBoolean) arguments.get(0).eval(context)).getValue()
//                    ? arguments.get(1) : arguments.get(2)).eval(context);
//        }
//    };
//    public static final HigherOrderFunction STRING = new HigherOrderFunction(Commons.class, "string",
//            A, FunckyListType.STRING) {
//        @Override
//        protected FunckyList apply(final ScriptContext context, final List<FunckyExpression> arguments) {
//                return FunckyJavaConverter.convert(arguments.getFirst().eval(context).toString());
//        }
//    };
//    public static final HigherOrderFunction NUMBER = new HigherOrderFunction(Commons.class, "number",
//            FunckyListType.STRING, FunckySimpleType.NUMBER) {
//        @Override
//        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
//                final FunckyList value = (FunckyList) arguments.getFirst().eval(context);
//                try {
//                    return new FunckyNumber(new BigDecimal(value.toString()));
//                } catch (final NumberFormatException e) {
//                    throw new SneakyRuntimeException(String.format(ERROR_INVALID_NUMBER, value));
//                }
//        }
//    };
//    public static final HigherOrderFunction ERROR = new HigherOrderFunction(Commons.class, "error",
//            FunckyListType.STRING, A) {
//        @Override
//        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
//            throw new SneakyRuntimeException(arguments.getFirst().eval(context).toString());
//        }
//    };
//    public static final HigherOrderFunction BOTTOM = new HigherOrderFunction(Commons.class, "bottom",
//            A, B) {
//        @Override
//        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
//            return apply(arguments.getFirst(), context);
//        }
//    };
//    private static final String ERROR_INVALID_NUMBER = "Invalid number `%1$s`";
//
//    public Commons() {
//        super(EQUAL, COMPARE, HASH, IF, STRING, NUMBER, ERROR, BOTTOM);
//    }
    public Commons(final FunckyEngine engine) {
        super(engine);
    }
}
