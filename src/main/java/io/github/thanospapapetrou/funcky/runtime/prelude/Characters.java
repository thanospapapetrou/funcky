package io.github.thanospapapetrou.funcky.runtime.prelude;

import io.github.thanospapapetrou.funcky.FunckyEngine;

public final class Characters extends FunckyLibrary {
    //    public static final HigherOrderFunction UPPERCASE = new HigherOrderFunction(Characters.class, "uppercase",
    //            FunckySimpleType.CHARACTER, FunckySimpleType.CHARACTER) {
    //        @Override
    //        protected FunckyCharacter apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            return new FunckyCharacter(
    //                    Character.toUpperCase(((FunckyCharacter) arguments.getFirst().eval(context)).getValue()));
    //        }
    //    };
    //    public static final HigherOrderFunction LOWERCASE = new HigherOrderFunction(Characters.class, "lowercase",
    //            FunckySimpleType.CHARACTER, FunckySimpleType.CHARACTER) {
    //        @Override
    //        protected FunckyCharacter apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            return new FunckyCharacter(
    //                    Character.toLowerCase(((FunckyCharacter) arguments.getFirst().eval(context)).getValue()));
    //        }
    //    };
    //    public static final HigherOrderFunction NUMBER = new HigherOrderFunction(Characters.class, "number",
    //            FunckySimpleType.CHARACTER, FunckySimpleType.NUMBER) {
    //        @Override
    //        protected FunckyNumber apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            return new FunckyNumber(new BigDecimal(((FunckyCharacter) arguments.getFirst().eval(context)).getValue()));
    //        }
    //    };
    //    public static final HigherOrderFunction CHARACTER = new HigherOrderFunction(Characters.class, "character",
    //            FunckySimpleType.NUMBER, FunckySimpleType.CHARACTER) {
    //        @Override
    //        protected FunckyCharacter apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            final FunckyNumber codePointNumber = (FunckyNumber) arguments.getFirst().eval(context);
    //            final int codePointInt =
    //                    requireInt(codePointNumber, String.format(ERROR_INVALID_UNICODE_CODE_POINT, codePointNumber));
    //            if ((codePointInt < Character.MIN_VALUE) || (codePointInt >= Character.MAX_VALUE + 1)) {
    //                throw new SneakyRuntimeException(String.format(ERROR_INVALID_UNICODE_CODE_POINT, codePointNumber));
    //            }
    //            return new FunckyCharacter((char) codePointInt);
    //        }
    //    };
    //    private static final String ERROR_INVALID_UNICODE_CODE_POINT = "Invalid Unicode code point `%1$s`";
    //
    //    public Characters() {
    //        super(UPPERCASE, LOWERCASE, NUMBER, CHARACTER);
    //    }

    public Characters(final FunckyEngine engine) {
        super(engine);
    }
}
