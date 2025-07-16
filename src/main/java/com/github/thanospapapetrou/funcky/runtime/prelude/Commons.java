package com.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.util.List;

import javax.script.ScriptContext;

import com.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import com.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import com.github.thanospapapetrou.funcky.runtime.FunckyList;
import com.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import com.github.thanospapapetrou.funcky.runtime.FunckyValue;
import com.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import com.github.thanospapapetrou.funcky.runtime.exceptions.SneakyFunckyRuntimeException;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import com.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public class Commons extends FunckyLibrary {
    private static final FunckyTypeVariable A = new FunckyTypeVariable();
    private static final FunckyTypeVariable B = new FunckyTypeVariable();
    public static final HigherOrderFunction EQUAL = new HigherOrderFunction(Commons.class, "equal",
            A, A, FunckySimpleType.BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            try {
                return arguments.get(0).eval(context).equals(arguments.get(1).eval(context))
                        ? FunckyBoolean.TRUE : FunckyBoolean.FALSE;
            } catch (final SneakyFunckyRuntimeException e) {
                throw (FunckyRuntimeException) e.getCause();
            }
        }
    };
    public static final HigherOrderFunction COMPARE = new HigherOrderFunction(Commons.class, "compare",
            A, A, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            try {
                return new FunckyNumber(new BigDecimal(
                        Integer.compare(((Comparable<FunckyValue>) arguments.get(0).eval(context))
                                .compareTo(arguments.get(1).eval(context)), 0)));
            } catch (final SneakyFunckyRuntimeException e) {
                throw (FunckyRuntimeException) e.getCause();
            }
        }
    };
    public static final HigherOrderFunction HASH = new HigherOrderFunction(Commons.class, "hash",
            A, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            try {
                return new FunckyNumber(new BigDecimal(arguments.get(0).eval(context).hashCode()));
            } catch (final SneakyFunckyRuntimeException e) {
                throw (FunckyRuntimeException) e.getCause();
            }
        }
    };
    public static final HigherOrderFunction IF = new HigherOrderFunction(Commons.class, "if",
            FunckySimpleType.BOOLEAN, A, A, A) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            return (((FunckyBoolean) arguments.get(0).eval(context)).getValue()
                    ? arguments.get(1) : arguments.get(2)).eval(context);
        }
    };
    public static final HigherOrderFunction STRING = new HigherOrderFunction(Commons.class, "string",
            A, FunckyListType.STRING) {
        @Override
        protected FunckyList apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            try {
                return arguments.get(0).getEngine().getConverter().convert(arguments.get(0).eval(context).toString());
            } catch (final SneakyFunckyRuntimeException e) {
                throw (FunckyRuntimeException) e.getCause();
            }
        }
    };
    public static final HigherOrderFunction NUMBER = new HigherOrderFunction(Commons.class, "number",
            FunckyListType.STRING, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            try {
                final FunckyList value = (FunckyList) arguments.get(0).eval(context);
                try {
                    return new FunckyNumber(new BigDecimal(value.toString()));
                } catch (final NumberFormatException e) {
                    throw new FunckyRuntimeException(String.format(ERROR_INVALID_NUMBER, value));
                }
            } catch (final SneakyFunckyRuntimeException e) {
                throw (FunckyRuntimeException) e.getCause();
            }
        }
    };
    public static final HigherOrderFunction ERROR = new HigherOrderFunction(Commons.class, "error",
            FunckyListType.STRING, A) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            throw new FunckyRuntimeException(arguments.get(0).eval(context).toString());
        }
    };
    public static final HigherOrderFunction BOTTOM = new HigherOrderFunction(Commons.class, "bottom",
            A, B) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments)
                throws FunckyRuntimeException {
            return apply(arguments.get(0), context);
        }
    };
    private static final String ERROR_INVALID_NUMBER = "Invalid number `%1$s`";

    public Commons() {
        super(EQUAL, COMPARE, HASH, IF, STRING, NUMBER, ERROR, BOTTOM);
    }
}
