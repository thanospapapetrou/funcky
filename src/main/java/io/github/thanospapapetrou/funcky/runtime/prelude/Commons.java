package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
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

public final class Commons extends FunckyLibrary {
    private static final String ERROR_INVALID_NUMBER = "Invalid number `%1$s`";

    private final FunckyTypeVariable $_a = new FunckyTypeVariable(engine);
    private final FunckyTypeVariable $_b = new FunckyTypeVariable(engine);
    public final HigherOrderFunction $equal = new HigherOrderFunction(engine, this, engine -> $_a, engine ->$_a, BOOLEAN) {
        @Override
        protected FunckyBoolean apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return (arguments.get(0).eval(context).equals(arguments.get(1).eval(context)) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(engine);
                }
    };
    public final HigherOrderFunction $compare = new HigherOrderFunction(engine, this, engine -> $_a, engine -> $_a, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, new BigDecimal(Integer.compare(
                    ((Comparable<FunckyValue>) arguments.get(0).eval(context))
                            .compareTo(arguments.get(1).eval(context)), 0)));
                }
    };
    public final HigherOrderFunction $hash = new HigherOrderFunction(engine, this, engine -> $_a, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(engine, new BigDecimal(arguments.getFirst().eval(context).hashCode()));
        }
    };
    public final HigherOrderFunction $if = new HigherOrderFunction(engine, this,
            BOOLEAN, engine -> $_a, engine -> $_a, engine -> $_a) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return (((FunckyBoolean) arguments.get(0).eval(context)).getValue()
                    ? arguments.get(1) : arguments.get(2)).eval(context);
        }
    };
    public final HigherOrderFunction $string = new HigherOrderFunction(engine, this, engine -> $_a, STRING) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return engine.getConverter().convert(arguments.getFirst().eval(context).toString());
        }
    };
    public final HigherOrderFunction $number = new HigherOrderFunction(engine, this, STRING, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
                final FunckyList value = (FunckyList) arguments.getFirst().eval(context);
                try {
                    return new FunckyNumber(engine, new BigDecimal(value.toString()));
                } catch (final NumberFormatException e) {
                    throw new SneakyRuntimeException(String.format(ERROR_INVALID_NUMBER, value));
                }
        }
    };
    public final HigherOrderFunction $error = new HigherOrderFunction(engine, this, STRING, engine -> $_a) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            throw new SneakyRuntimeException(arguments.getFirst().eval(context).toString());
        }
    };
    public final HigherOrderFunction $bottom = new HigherOrderFunction(engine, this, engine -> $_a, engine -> $_b) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return apply(arguments.getFirst(), context);
        }
    };

    public Commons(final FunckyEngine engine) {
        super(engine);
    }
}
