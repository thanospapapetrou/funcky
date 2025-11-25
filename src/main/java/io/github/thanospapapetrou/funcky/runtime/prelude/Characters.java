package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.math.BigDecimal;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.CHARACTER;
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.NUMBER;

public final class Characters extends FunckyLibrary {
    private static final String ERROR_INVALID_UNICODE_CODE_POINT = "Invalid Unicode code point `%1$s`";

    public final HigherOrderFunction $uppercase = new HigherOrderFunction(this, CHARACTER, CHARACTER) {
        @Override
        protected FunckyCharacter apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyCharacter(Character.toUpperCase(((FunckyCharacter) arguments.getFirst().eval(context))
                    .getValue()));
        }
    };
    public final HigherOrderFunction $lowercase = new HigherOrderFunction(this, CHARACTER, CHARACTER) {
        @Override
        protected FunckyCharacter apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyCharacter(
                    Character.toLowerCase(((FunckyCharacter) arguments.getFirst().eval(context)).getValue()));
        }
    };
    public final HigherOrderFunction $number = new HigherOrderFunction(this, CHARACTER, NUMBER) {
        @Override
        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyNumber(new BigDecimal(((FunckyCharacter) arguments.getFirst().eval(context)).getValue()));
        }
    };
    public final HigherOrderFunction $character = new HigherOrderFunction(this, NUMBER, CHARACTER) {
        @Override
        protected FunckyCharacter apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyNumber codePointNumber = (FunckyNumber) arguments.getFirst().eval(context);
            final int codePointInt =
                    requireInt(codePointNumber, String.format(ERROR_INVALID_UNICODE_CODE_POINT, codePointNumber));
            if ((codePointInt < Character.MIN_VALUE) || (codePointInt >= Character.MAX_VALUE + 1)) {
                throw new SneakyRuntimeException(String.format(ERROR_INVALID_UNICODE_CODE_POINT, codePointNumber));
            }
            return new FunckyCharacter((char) codePointInt);
        }
    };

    public Characters(final FunckyEngine engine) {
        super(engine);
    }
}
