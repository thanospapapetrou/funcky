package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public final class Numbers extends FunckyLibrary {
    public static final HigherOrderFunction PLUS = new HigherOrderFunction(Numbers.class, "plus",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return (FunckyNumber) arguments.getFirst().eval(context);
        }
    };
    public static final HigherOrderFunction MINUS = new HigherOrderFunction(Numbers.class, "minus",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(((FunckyNumber) arguments.getFirst().eval(context)).getValue().negate());
        }
    };
    public static final HigherOrderFunction ADD = new HigherOrderFunction(Numbers.class, "add",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(((FunckyNumber) arguments.get(0).eval(context)).getValue()
                    .add(((FunckyNumber) arguments.get(1).eval(context)).getValue()));
        }
    };
    public static final HigherOrderFunction SUBTRACT = new HigherOrderFunction(Numbers.class, "subtract",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(((FunckyNumber) arguments.get(0).eval(context)).getValue()
                    .subtract(((FunckyNumber) arguments.get(1).eval(context)).getValue()));
        }
    };
    public static final HigherOrderFunction MULTIPLY = new HigherOrderFunction(Numbers.class, "multiply",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(((FunckyNumber) arguments.get(0).eval(context)).getValue()
                    .multiply(((FunckyNumber) arguments.get(1).eval(context)).getValue()));
        }
    };
    public static final HigherOrderFunction DIVIDE = new HigherOrderFunction(Numbers.class, "divide",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER,
            FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final BigDecimal divisor = ((FunckyNumber) arguments.get(1).eval(context)).getValue();
            if (divisor.compareTo(BigDecimal.ZERO) == 0) {
                throw new SneakyRuntimeException(ERROR_DIVISION_BY_ZERO);
            }
            final FunckyNumber scale = (FunckyNumber) arguments.get(2).eval(context);
            final FunckyNumber roundingMode = (FunckyNumber) arguments.get(3).eval(context);
            try {
                return new FunckyNumber(((FunckyNumber) arguments.getFirst().eval(context)).getValue()
                        .divide(divisor, requireInt(scale, String.format(ERROR_INVALID_SCALE, scale)),
                                requireEnum(roundingMode, RoundingMode.class,
                                        String.format(ERROR_INVALID_ROUNDING_MODE, roundingMode))));
            } catch (final ArithmeticException e) {
                throw new SneakyRuntimeException(String.format(ERROR_INSUFFICIENT_SCALE, scale, roundingMode));
            }
        }
    };
    public static final HigherOrderFunction BYTE = new HigherOrderFunction(Numbers.class, "byte",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context)).getValue()
                    .byteValue()));
        }
    };
    public static final HigherOrderFunction SHORT = new HigherOrderFunction(Numbers.class, "short",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context)).getValue()
                    .shortValue()));
        }
    };
    public static final HigherOrderFunction INT = new HigherOrderFunction(Numbers.class, "int",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context)).getValue()
                    .intValue()));
        }
    };
    public static final HigherOrderFunction LONG = new HigherOrderFunction(Numbers.class, "long",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context)).getValue()
                    .longValue()));
        }
    };
    public static final HigherOrderFunction FLOAT = new HigherOrderFunction(Numbers.class, "float",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context)).getValue()
                    .floatValue()));
        }
    };
    public static final HigherOrderFunction DOUBLE = new HigherOrderFunction(Numbers.class, "double",
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context)).getValue()
                    .doubleValue()));
        }
    };

    private static final String ERROR_DIVISION_BY_ZERO = "Division by zero";
    private static final String ERROR_INSUFFICIENT_SCALE = "Insufficient scale `%1$s` for rounding mode `%2$s`";
    private static final String ERROR_INVALID_SCALE = "Invalid scale `%1$s`, should be an int";
    private static final String ERROR_INVALID_ROUNDING_MODE = "Invalid rounding mode `%1$s`";

    public Numbers() {
        super(PLUS, MINUS, ADD, SUBTRACT, MULTIPLY, DIVIDE, BYTE, SHORT, INT, LONG, FLOAT, DOUBLE);
    }
}
