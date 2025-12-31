package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Characters
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER

class CharactersSpec extends BaseSpec {
    @Unroll('Test uppercase (expression: #expression)')
    def 'Test uppercase'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                              || result
        '"funcky:commons".string "funcky:characters".uppercase' || toFuncky('"funcky:characters".uppercase')
        '"funcky:types".type "funcky:characters".uppercase'     || FUNCTION(CHARACTER, CHARACTER).apply(engine.context)
        '"funcky:characters".uppercase \'A\''                   || new FunckyCharacter(engine.context, 'A' as char)
        '"funcky:characters".uppercase \'a\''                   || new FunckyCharacter(engine.context, 'A' as char)
        '"funcky:characters".uppercase \'$\''                   || new FunckyCharacter(engine.context, '$' as char)
    }

    @Unroll('Test lowercase (expression: #expression)')
    def 'Test lowercase'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                              || result
        '"funcky:commons".string "funcky:characters".lowercase' || toFuncky('"funcky:characters".lowercase')
        '"funcky:types".type "funcky:characters".lowercase'     || FUNCTION(CHARACTER, CHARACTER).apply(engine.context)
        '"funcky:characters".lowercase \'A\''                   || new FunckyCharacter(engine.context, 'a' as char)
        '"funcky:characters".lowercase \'a\''                   || new FunckyCharacter(engine.context, 'a' as char)
        '"funcky:characters".lowercase \'$\''                   || new FunckyCharacter(engine.context, '$' as char)
    }

    @Unroll('Test number (expression: #expression)')
    def 'Test number'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                           || result
        '"funcky:commons".string "funcky:characters".number' || toFuncky('"funcky:characters".number')
        '"funcky:types".type "funcky:characters".number'     || FUNCTION(CHARACTER, NUMBER).apply(engine.context)
        '"funcky:characters".number \'A\''                   || new FunckyNumber(engine.context, 65.0G)
        '"funcky:characters".number \'a\''                   || new FunckyNumber(engine.context, 97.0G)
        '"funcky:characters".number \'\\\\\''                || new FunckyNumber(engine.context, 92.0G)
        '"funcky:characters".number \'\\t\''                 || new FunckyNumber(engine.context, 9.0G)
        '"funcky:characters".number \'\\b\''                 || new FunckyNumber(engine.context, 8.0G)
        '"funcky:characters".number \'\\n\''                 || new FunckyNumber(engine.context, 10.0G)
        '"funcky:characters".number \'\\r\''                 || new FunckyNumber(engine.context, 13.0G)
        '"funcky:characters".number \'\\f\''                 || new FunckyNumber(engine.context, 12.0G)
        '"funcky:characters".number \'\\\'\''                || new FunckyNumber(engine.context, 39.0G)
        '"funcky:characters".number \'\\"\''                 || new FunckyNumber(engine.context, 34.0G)
        '"funcky:characters".number \'\\0\''                 || new FunckyNumber(engine.context, 0.0G)
        '"funcky:characters".number \'\\1\''                 || new FunckyNumber(engine.context, 1.0G)
        '"funcky:characters".number \'\\01\''                || new FunckyNumber(engine.context, 1.0G)
        '"funcky:characters".number \'\\10\''                || new FunckyNumber(engine.context, 8.0G)
        '"funcky:characters".number \'\\u0000\''             || new FunckyNumber(engine.context, 0.0G)
        '"funcky:characters".number \'\\u0001\''             || new FunckyNumber(engine.context, 1.0G)
        '"funcky:characters".number \'\\u000F\''             || new FunckyNumber(engine.context, 15.0G)
        '"funcky:characters".number \'\\u000f\''             || new FunckyNumber(engine.context, 15.0G)
        '"funcky:characters".number \'\\u0010\''             || new FunckyNumber(engine.context, 16.0G)
    }

    @Unroll('Test character (expression: #expression)')
    def 'Test character'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                              || result
        '"funcky:commons".string "funcky:characters".character' || toFuncky('"funcky:characters".character')
        '"funcky:types".type "funcky:characters".character'     || FUNCTION(NUMBER, CHARACTER).apply(engine.context)
        '"funcky:characters".character 65'                      || new FunckyCharacter(engine.context, 'A' as char)
        '"funcky:characters".character 97'                      || new FunckyCharacter(engine.context, 'a' as char)
        '"funcky:characters".character 0'                       || new FunckyCharacter(engine.context, '\u0000' as char)
        '"funcky:characters".character 65535'                   || new FunckyCharacter(engine.context, '\uFFFF' as char)
    }

    @Unroll('Test character (runtime error, expression: #expression)')
    def 'Test character (runtime error)'(final String expression, final String message) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(message)
        where:
        expression                            || message
        '"funcky:characters".character 0.1'   || String.format(Characters.ERROR_INVALID_UNICODE_CODE_POINT, 0.1G)
        '"funcky:characters".character 65536' || String.format(Characters.ERROR_INVALID_UNICODE_CODE_POINT, 65536)
        '"funcky:characters".character -1'    || String.format(Characters.ERROR_INVALID_UNICODE_CODE_POINT, -1)
    }
}
