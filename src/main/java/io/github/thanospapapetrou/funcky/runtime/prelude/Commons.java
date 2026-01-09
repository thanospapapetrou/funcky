package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.util.List;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
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

    private final FunckyTypeVariable a = new FunckyTypeVariable(context);
    private final FunckyTypeVariable b = new FunckyTypeVariable(context);
    private final FunckyLiteral nil = new FunckyLiteral(new FunckyRecord(context));
    public final HigherOrderFunction equal = new HigherOrderFunction(context, a, a, BOOLEAN) {
        @Override
        public FunckyBoolean apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return (arguments.get(0).eval(context).equals(arguments.get(1).eval(context)) ? FunckyBoolean.TRUE
                    : FunckyBoolean.FALSE).apply(context);
                }
    };
    public final HigherOrderFunction compare = new HigherOrderFunction(context, a, a, NUMBER) {
        @Override
        public FunckyNumber apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyNumber(context, new BigDecimal(Integer.compare(
                    ((Comparable<FunckyValue>) arguments.get(0).eval(context))
                            .compareTo(arguments.get(1).eval(context)), 0)));
                }
    };
    public final HigherOrderFunction hash = new HigherOrderFunction(context, a, NUMBER) {
        @Override
        public FunckyNumber apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyNumber(context, new BigDecimal(arguments.getFirst().eval(context).hashCode()));
        }
    };
    public final HigherOrderFunction _if = new HigherOrderFunction(context, BOOLEAN, a, a, a) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return (((FunckyBoolean) arguments.get(0).eval(context)).getValue()
                    ? arguments.get(1) : arguments.get(2)).eval(context);
        }
    };
    public final HigherOrderFunction string = new HigherOrderFunction(context, a, STRING) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return context.getEngine().toFuncky(arguments.getFirst().eval(context).toString());
        }
    };
    public final HigherOrderFunction number = new HigherOrderFunction(context, STRING, NUMBER) {
        @Override
        public FunckyNumber apply(final List<FunckyExpression> arguments, final FunckyContext context) {
                final FunckyList value = (FunckyList) arguments.getFirst().eval(context);
                try {
                    return new FunckyNumber(context, new BigDecimal(value.toString()));
                } catch (final NumberFormatException e) {
                    throw new SneakyRuntimeException(String.format(ERROR_INVALID_NUMBER, value));
                }
        }
    };
    public final HigherOrderFunction exit = new HigherOrderFunction(context, NUMBER, IO(UNIT)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            java.lang.System.exit(((FunckyNumber) arguments.getFirst().eval(context)).getValue().intValue());
            return new FunckyMonad(context, IO(UNIT).apply(context), () -> nil);
        }
    };
    public final HigherOrderFunction error = new HigherOrderFunction(context, STRING, a) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            throw new SneakyRuntimeException(arguments.getFirst().eval(context).toString());
        }
    };
    public final HigherOrderFunction bottom = new HigherOrderFunction(context, a, b) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return apply(arguments.getFirst(), context);
        }
    };

    public Commons(final FunckyContext context) {
        super(context);
    }
}
