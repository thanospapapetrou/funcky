package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Characters
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType
import spock.lang.Unroll

class CharactersSpec extends BaseSpec {
    @Unroll('Test uppercase (expression: #expression)')
    def 'Test uppercase'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                          || result
        '"funcky:characters".uppercase'                     || Characters.UPPERCASE
        '"funcky:types".type "funcky:characters".uppercase' || new FunckyFunctionType(FunckySimpleType.CHARACTER, FunckySimpleType.CHARACTER)
        '"funcky:characters".uppercase \'A\''               || new FunckyCharacter('A' as char)
        '"funcky:characters".uppercase \'a\''               || new FunckyCharacter('A' as char)
        '"funcky:characters".uppercase \'$\''               || new FunckyCharacter('$' as char)
    }

    @Unroll('Test lowercase (expression: #expression)')
    def 'Test lowercase'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                          || result
        '"funcky:characters".lowercase'                     || Characters.LOWERCASE
        '"funcky:types".type "funcky:characters".lowercase' || new FunckyFunctionType(FunckySimpleType.CHARACTER, FunckySimpleType.CHARACTER)
        '"funcky:characters".lowercase \'A\''               || new FunckyCharacter('a' as char)
        '"funcky:characters".lowercase \'a\''               || new FunckyCharacter('a' as char)
        '"funcky:characters".lowercase \'$\''               || new FunckyCharacter('$' as char)
    }

    @Unroll('Test number (expression: #expression)')
    def 'Test number'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                       || result
        '"funcky:characters".number'                     || Characters.NUMBER
        '"funcky:types".type "funcky:characters".number' || new FunckyFunctionType(FunckySimpleType.CHARACTER, FunckySimpleType.NUMBER)
        '"funcky:characters".number \'A\''               || new FunckyNumber(65.0G)
        '"funcky:characters".number \'a\''               || new FunckyNumber(97.0G)
        '"funcky:characters".number \'\\\\\''            || new FunckyNumber(92.0G)
        '"funcky:characters".number \'\\t\''             || new FunckyNumber(9.0G)
        '"funcky:characters".number \'\\b\''             || new FunckyNumber(8.0G)
        '"funcky:characters".number \'\\n\''             || new FunckyNumber(10.0G)
        '"funcky:characters".number \'\\r\''             || new FunckyNumber(13.0G)
        '"funcky:characters".number \'\\f\''             || new FunckyNumber(12.0G)
        '"funcky:characters".number \'\\\'\''            || new FunckyNumber(39.0G)
        '"funcky:characters".number \'\\"\''             || new FunckyNumber(34.0G)
        '"funcky:characters".number \'\\0\''             || new FunckyNumber(0.0G)
        '"funcky:characters".number \'\\1\''             || new FunckyNumber(1.0G)
        '"funcky:characters".number \'\\01\''            || new FunckyNumber(1.0G)
        '"funcky:characters".number \'\\10\''            || new FunckyNumber(8.0G)
        '"funcky:characters".number \'\\u0000\''         || new FunckyNumber(0.0G)
        '"funcky:characters".number \'\\u0001\''         || new FunckyNumber(1.0G)
        '"funcky:characters".number \'\\u000F\''         || new FunckyNumber(15.0G)
        '"funcky:characters".number \'\\u000f\''         || new FunckyNumber(15.0G)
        '"funcky:characters".number \'\\u0010\''         || new FunckyNumber(16.0G)
    }

    @Unroll('Test character (expression: #expression)')
    def 'Test character'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                          || result
        '"funcky:characters".character'                     || Characters.CHARACTER
        '"funcky:types".type "funcky:characters".character' || new FunckyFunctionType(FunckySimpleType.NUMBER, FunckySimpleType.CHARACTER)
        '"funcky:characters".character 65'                  || new FunckyCharacter('A' as char)
        '"funcky:characters".character 97'                  || new FunckyCharacter('a' as char)
        '"funcky:characters".character 0'                   || new FunckyCharacter('\u0000' as char)
        '"funcky:characters".character 65535'               || new FunckyCharacter('\uFFFF' as char)
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
