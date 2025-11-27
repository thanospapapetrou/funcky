package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public final class Numbers extends FunckyLibrary {
    private static final String ERROR_DIVISION_BY_ZERO = "Division by zero";
    private static final String ERROR_INSUFFICIENT_SCALE = "Insufficient scale `%1$s` for rounding mode `%2$s`";
    private static final String ERROR_INVALID_SCALE = "Invalid scale `%1$s`, should be an int";
    private static final String ERROR_INVALID_ROUNDING_MODE = "Invalid rounding mode `%1$s`";

    public final HigherOrderFunction $plus = new HigherOrderFunction(this,
            FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return (FunckyNumber) arguments.getFirst().eval();
        }
    };
    public final HigherOrderFunction $minus =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(((FunckyNumber) arguments.getFirst().eval()).getValue().negate());
        }
    };
    public final HigherOrderFunction $add =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(((FunckyNumber) arguments.get(0).eval()).getValue()
                    .add(((FunckyNumber) arguments.get(1).eval()).getValue()));
        }
    };
    public final HigherOrderFunction $subtract =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(((FunckyNumber) arguments.get(0).eval()).getValue()
                    .subtract(((FunckyNumber) arguments.get(1).eval()).getValue()));
        }
    };
    public final HigherOrderFunction $multiply =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(((FunckyNumber) arguments.get(0).eval()).getValue()
                    .multiply(((FunckyNumber) arguments.get(1).eval()).getValue()));
        }
    };
    public final HigherOrderFunction $divide =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER,
                    FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            final BigDecimal divisor = ((FunckyNumber) arguments.get(1).eval()).getValue();
            if (divisor.compareTo(BigDecimal.ZERO) == 0) {
                throw new SneakyRuntimeException(ERROR_DIVISION_BY_ZERO);
            }
            final FunckyNumber scale = (FunckyNumber) arguments.get(2).eval();
            final FunckyNumber roundingMode = (FunckyNumber) arguments.get(3).eval();
            try {
                return new FunckyNumber(((FunckyNumber) arguments.getFirst().eval()).getValue()
                        .divide(divisor, requireInt(scale, String.format(ERROR_INVALID_SCALE, scale)),
                                requireEnum(roundingMode, RoundingMode.class,
                                        String.format(ERROR_INVALID_ROUNDING_MODE, roundingMode))));
            } catch (final ArithmeticException e) {
                throw new SneakyRuntimeException(String.format(ERROR_INSUFFICIENT_SCALE, scale, roundingMode));
            }
        }
    };
    public final HigherOrderFunction $byte =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval())
                    .getValue().byteValue()));
        }
    };
    public final HigherOrderFunction $short =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval())
                    .getValue().shortValue()));
        }
    };
    public final HigherOrderFunction $int =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval())
                    .getValue().intValue()));
        }
    };
    public final HigherOrderFunction $long =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval())
                    .getValue().longValue()));
        }
    };
    public final HigherOrderFunction $float =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval())
                    .getValue().floatValue()));
        }
    };
    public final HigherOrderFunction $double =
            new HigherOrderFunction(this, FunckySimpleType.NUMBER, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyNumber) arguments.getFirst().eval())
                    .getValue().doubleValue()));
        }
    };

    public Numbers(final FunckyEngine engine) {
        super(engine);
    }
}
