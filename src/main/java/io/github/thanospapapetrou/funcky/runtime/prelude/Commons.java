package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyJavaConverter;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.STRING;
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.BOOLEAN;
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.NUMBER;

public non-sealed class Commons extends FunckyLibrary {
    private static final String ERROR_INVALID_NUMBER = "Invalid number `%1$s`";

    private final FunckyTypeVariable $_a = new FunckyTypeVariable();
    private final FunckyTypeVariable $_b = new FunckyTypeVariable();
    public final HigherOrderFunction $equal = new HigherOrderFunction(this, $_a, $_a, BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return arguments.get(0).eval(context).equals(arguments.get(1).eval(context)) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE;
                }
    };
    public final HigherOrderFunction $compare = new HigherOrderFunction(this, $_a, $_a, NUMBER) {
        @Override
        @SuppressWarnings("unchecked")
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(Integer
                    .compare(((Comparable<FunckyValue>) arguments.get(0).eval(context))
                            .compareTo(arguments.get(1).eval(context)), 0)));
                }
    };
    public final HigherOrderFunction $hash = new HigherOrderFunction(this, $_a, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(arguments.getFirst().eval(context).hashCode()));
        }
    };
    public final HigherOrderFunction $if = new HigherOrderFunction(this,
            BOOLEAN, $_a, $_a, $_a) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return (((FunckyBoolean) arguments.get(0).eval(context)).getValue()
                    ? arguments.get(1) : arguments.get(2)).eval(context);
        }
    };
    public final HigherOrderFunction $string = new HigherOrderFunction(this, $_a, STRING) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyJavaConverter().convert(arguments.getFirst().eval(context).toString());
        }
    };
    public final HigherOrderFunction $number = new HigherOrderFunction(this, STRING, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
                final FunckyList value = (FunckyList) arguments.getFirst().eval(context);
                try {
                    return new FunckyNumber(new BigDecimal(value.toString()));
                } catch (final NumberFormatException e) {
                    throw new SneakyRuntimeException(String.format(ERROR_INVALID_NUMBER, value));
                }
        }
    };
    public final HigherOrderFunction $error = new HigherOrderFunction(this, STRING, $_a) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            throw new SneakyRuntimeException(arguments.getFirst().eval(context).toString());
        }
    };
    public final HigherOrderFunction $bottom = new HigherOrderFunction(this, $_a, $_b) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return apply(arguments.getFirst(), context);
        }
    };

    public Commons(final FunckyEngine engine) {
        super(engine);
    }
}
