package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER;

public final class Numbers extends FunckyLibrary {
    private static final String ERROR_DIVISION_BY_ZERO = "Division by zero";
    private static final String ERROR_INSUFFICIENT_SCALE = "Insufficient scale `%1$s` for rounding mode `%2$s`";
    private static final String ERROR_INVALID_SCALE = "Invalid scale `%1$s`, should be an int";
    private static final String ERROR_INVALID_ROUNDING_MODE = "Invalid rounding mode `%1$s`";

    public final HigherOrderFunction $plus = new HigherOrderFunction(engine, this, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return (FunckyNumber) arguments.getFirst().eval(context);
        }
    };
    public final HigherOrderFunction $minus = new HigherOrderFunction(engine, this, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, ((FunckyNumber) arguments.getFirst().eval(context)).getValue().negate());
        }
    };
    public final HigherOrderFunction $add = new HigherOrderFunction(engine, this, NUMBER, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, ((FunckyNumber) arguments.get(0).eval(context)).getValue()
                    .add(((FunckyNumber) arguments.get(1).eval(context)).getValue()));
        }
    };
    public final HigherOrderFunction $subtract = new HigherOrderFunction(engine, this, NUMBER, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, ((FunckyNumber) arguments.get(0).eval(context)).getValue()
                    .subtract(((FunckyNumber) arguments.get(1).eval(context)).getValue()));
        }
    };
    public final HigherOrderFunction $multiply = new HigherOrderFunction(engine, this, NUMBER, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, ((FunckyNumber) arguments.get(0).eval(context)).getValue()
                    .multiply(((FunckyNumber) arguments.get(1).eval(context)).getValue()));
        }
    };
    public final HigherOrderFunction $divide = new HigherOrderFunction(engine, this, NUMBER, NUMBER, NUMBER, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final BigDecimal divisor = ((FunckyNumber) arguments.get(1).eval(context)).getValue();
            if (divisor.compareTo(BigDecimal.ZERO) == 0) {
                throw new SneakyRuntimeException(ERROR_DIVISION_BY_ZERO);
            }
            final FunckyNumber scale = (FunckyNumber) arguments.get(2).eval(context);
            final FunckyNumber roundingMode = (FunckyNumber) arguments.get(3).eval(context);
            try {
                return new FunckyNumber(engine, ((FunckyNumber) arguments.getFirst().eval(context)).getValue()
                        .divide(divisor, requireInt(scale, String.format(ERROR_INVALID_SCALE, scale)),
                                requireEnum(roundingMode, RoundingMode.class,
                                        String.format(ERROR_INVALID_ROUNDING_MODE, roundingMode))));
            } catch (final ArithmeticException e) {
                throw new SneakyRuntimeException(String.format(ERROR_INSUFFICIENT_SCALE, scale, roundingMode));
            }
        }
    };
    public final HigherOrderFunction $byte = new HigherOrderFunction(engine, this, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context))
                    .getValue().byteValue()));
        }
    };
    public final HigherOrderFunction $short = new HigherOrderFunction(engine, this, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context))
                    .getValue().shortValue()));
        }
    };
    public final HigherOrderFunction $int = new HigherOrderFunction(engine, this, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context))
                    .getValue().intValue()));
        }
    };
    public final HigherOrderFunction $long = new HigherOrderFunction(engine, this, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context))
                    .getValue().longValue()));
        }
    };
    public final HigherOrderFunction $float = new HigherOrderFunction(engine, this, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context))
                    .getValue().floatValue()));
        }
    };
    public final HigherOrderFunction $double = new HigherOrderFunction(engine, this, NUMBER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, new BigDecimal(((FunckyNumber) arguments.getFirst().eval(context))
                    .getValue().doubleValue()));
        }
    };

    public Numbers(final FunckyEngine engine) {
        super(engine);
    }
}
