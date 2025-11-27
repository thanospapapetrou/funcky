package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.util.List;

import io.github.thanospapapetrou.funcky.FunckyEngine;
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
    private static final String ERROR_INVALID_NUMBER = "Invalid number `%1$s`";

    private final FunckyTypeVariable $_a = new FunckyTypeVariable();
    private final FunckyTypeVariable $_b = new FunckyTypeVariable();
    public final HigherOrderFunction $equal = new HigherOrderFunction(this, $_a, $_a, FunckySimpleType.BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final List<FunckyExpression> arguments) {
            return arguments.get(0).eval().equals(arguments.get(1).eval()) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE;
                }
    };
    public final HigherOrderFunction $compare = new HigherOrderFunction(this, $_a, $_a, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(Integer.compare(((Comparable<FunckyValue>) arguments.get(0).eval())
                    .compareTo(arguments.get(1).eval()), 0)));
                }
    };
    public final HigherOrderFunction $hash = new HigherOrderFunction(this, $_a, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(arguments.getFirst().eval().hashCode()));
        }
    };
    public final HigherOrderFunction $if = new HigherOrderFunction(this,
            FunckySimpleType.BOOLEAN, $_a, $_a, $_a) {
        @Override
        protected FunckyValue apply(final List<FunckyExpression> arguments) {
            return (((FunckyBoolean) arguments.get(0).eval()).getValue()
                    ? arguments.get(1) : arguments.get(2)).eval();
        }
    };
    public final HigherOrderFunction $string = new HigherOrderFunction(this, $_a, FunckyListType.STRING) {
        @Override
        protected FunckyList apply(final List<FunckyExpression> arguments) {
            return FunckyList.string(arguments.getFirst().eval().toString());
        }
    };
    public final HigherOrderFunction $number = new HigherOrderFunction(this,
            FunckyListType.STRING, FunckySimpleType.NUMBER) {
        @Override
        protected FunckyNumber apply(final List<FunckyExpression> arguments) {
            final FunckyList value = (FunckyList) arguments.getFirst().eval();
                try {
                    return new FunckyNumber(new BigDecimal(value.toString()));
                } catch (final NumberFormatException e) {
                    throw new SneakyRuntimeException(String.format(ERROR_INVALID_NUMBER, value));
                }
        }
    };
    public final HigherOrderFunction $error = new HigherOrderFunction(this, FunckyListType.STRING, $_a) {
        @Override
        protected FunckyValue apply(final List<FunckyExpression> arguments) {
            throw new SneakyRuntimeException(arguments.getFirst().eval().toString());
        }
    };
    public final HigherOrderFunction $bottom = new HigherOrderFunction(this, $_a, $_b) {
        @Override
        protected FunckyValue apply(final List<FunckyExpression> arguments) {
            return apply(arguments.getFirst());
        }
    };

    public Commons(final FunckyEngine engine) {
        super(engine);
    }
}
