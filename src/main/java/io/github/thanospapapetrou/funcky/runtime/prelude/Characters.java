package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER;

public final class Characters extends FunckyLibrary {
    private static final String ERROR_INVALID_UNICODE_CODE_POINT = "Invalid Unicode code point `%1$s`";

    public final HigherOrderFunction uppercase = new HigherOrderFunction(engine, CHARACTER, CHARACTER) {
        @Override
        public FunckyCharacter apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return new FunckyCharacter(engine,
                    Character.toUpperCase(((FunckyCharacter) arguments.getFirst().eval(context)).getValue()));
        }
    };
    public final HigherOrderFunction lowercase = new HigherOrderFunction(engine, CHARACTER, CHARACTER) {
        @Override
        public FunckyCharacter apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return new FunckyCharacter(engine,
                    Character.toLowerCase(((FunckyCharacter) arguments.getFirst().eval(context)).getValue()));
        }
    };
    public final HigherOrderFunction number = new HigherOrderFunction(engine, CHARACTER, NUMBER) {
        @Override
        public FunckyNumber apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return new FunckyNumber(engine,
                    new BigDecimal(((FunckyCharacter) arguments.getFirst().eval(context)).getValue()));
        }
    };
    public final HigherOrderFunction character = new HigherOrderFunction(engine, NUMBER, CHARACTER) {
        @Override
        public FunckyCharacter apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            final FunckyNumber codePointNumber = (FunckyNumber) arguments.getFirst().eval(context);
            final int codePointInt =
                    requireInt(codePointNumber, String.format(ERROR_INVALID_UNICODE_CODE_POINT, codePointNumber));
            if ((codePointInt < Character.MIN_VALUE) || (codePointInt >= Character.MAX_VALUE + 1)) {
                throw new SneakyRuntimeException(String.format(ERROR_INVALID_UNICODE_CODE_POINT, codePointNumber));
            }
            return new FunckyCharacter(engine, (char) codePointInt);
        }
    };

    public Characters(final FunckyEngine engine) {
        super(engine);
    }
}
