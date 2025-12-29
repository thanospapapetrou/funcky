package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyMonad;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType.IO;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType.UNIT;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.BOOLEAN;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER;

public final class Commons extends FunckyLibrary {
    private static final String ERROR_INVALID_NUMBER = "Invalid number `%1$s`";

    private final FunckyTypeVariable a = new FunckyTypeVariable(engine);
    private final FunckyTypeVariable b = new FunckyTypeVariable(engine);
    public final HigherOrderFunction equal = new HigherOrderFunction(engine, engine -> a, engine -> a, BOOLEAN) {
        @Override
        public FunckyBoolean apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return (arguments.get(0).eval(context).equals(arguments.get(1).eval(context)) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(engine);
                }
    };
    public final HigherOrderFunction compare = new HigherOrderFunction(engine, engine -> a, engine -> a, NUMBER) {
        @Override
        public FunckyNumber apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return new FunckyNumber(engine, new BigDecimal(Integer.compare(
                    ((Comparable<FunckyValue>) arguments.get(0).eval(context))
                            .compareTo(arguments.get(1).eval(context)), 0)));
                }
    };
    public final HigherOrderFunction hash = new HigherOrderFunction(engine, engine -> a, NUMBER) {
        @Override
        public FunckyNumber apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return new FunckyNumber(engine, new BigDecimal(arguments.getFirst().eval(context).hashCode()));
        }
    };
    public final HigherOrderFunction _if = new HigherOrderFunction(engine,
            BOOLEAN, engine -> a, engine -> a, engine -> a) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return (((FunckyBoolean) arguments.get(0).eval(context)).getValue()
                    ? arguments.get(1) : arguments.get(2)).eval(context);
        }
    };
    public final HigherOrderFunction string = new HigherOrderFunction(engine, engine -> a, STRING) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return engine.toFuncky(arguments.getFirst().eval(context).toString());
        }
    };
    public final HigherOrderFunction number = new HigherOrderFunction(engine, STRING, NUMBER) {
        @Override
        public FunckyNumber apply(final List<FunckyExpression> arguments, final ScriptContext context) {
                final FunckyList value = (FunckyList) arguments.getFirst().eval(context);
                try {
                    return new FunckyNumber(engine, new BigDecimal(value.toString()));
                } catch (final NumberFormatException e) {
                    throw new SneakyRuntimeException(String.format(ERROR_INVALID_NUMBER, value));
                }
        }
    };
    public final HigherOrderFunction exit = new HigherOrderFunction(engine,
            NUMBER, IO(UNIT)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            System.exit(((FunckyNumber) arguments.getFirst().eval(context)).getValue().intValue());
            return new FunckyMonad(engine, IO(UNIT).apply(engine),
                    () -> new FunckyLiteral(engine, new FunckyRecord(engine, UNIT.apply(engine),
                            List.of())));
        }
    };
    public final HigherOrderFunction error = new HigherOrderFunction(engine, STRING, engine -> a) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            throw new SneakyRuntimeException(arguments.getFirst().eval(context).toString());
        }
    };
    public final HigherOrderFunction bottom = new HigherOrderFunction(engine, engine -> a, engine -> b) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return apply(arguments.getFirst(), context);
        }
    };

    public Commons(final FunckyEngine engine) {
        super(engine);
    }
}
