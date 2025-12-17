package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Numbers
import spock.lang.Unroll

import java.math.RoundingMode

import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.FALSE
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER

class NumbersSpec extends BaseSpec {
    @Unroll('Test rounding modes (expression: #expression)')
    def 'Test rounding modes'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression << (RoundingMode.values()*.name().collect { "\"funcky:numbers\".$it" } + RoundingMode.values()*.name().collect { "\"funcky:types\".type (\"funcky:numbers\".$it)" })
        result << (RoundingMode.values()*.ordinal().collect(BigDecimal.&new).collect({new FunckyNumber(engine, it)}) + ([NUMBER.apply(engine)] * RoundingMode.values().size()))
    }

    @Unroll('Test plus (expression: #expression)')
    def 'Test plus'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                  || result
        '"funcky:numbers".plus'                     || new Numbers(engine).$plus
        '"funcky:types".type "funcky:numbers".plus' || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:numbers".plus 0'                   || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".plus 1'                   || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".plus -1'                  || new FunckyNumber(engine, -1.0G)
    }

    @Unroll('Test minus (expression: #expression)')
    def 'Test minus'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                   || result
        '"funcky:numbers".minus'                     || new Numbers(engine).$minus
        '"funcky:types".type "funcky:numbers".minus' || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:numbers".minus 0'                   || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".minus 1'                   || new FunckyNumber(engine, -1.0G)
        '"funcky:numbers".minus -1'                  || new FunckyNumber(engine, 1.0G)
    }

    @Unroll('Test round (expression: #expression)')
    def 'Test round'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                 || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:numbers".round)'                                    || FALSE.apply(engine)
        '"funcky:types".type "funcky:numbers".round'                                                               || FUNCTION(NUMBER, NUMBER, NUMBER, NUMBER).apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:numbers".round ("funcky:commons".error "foo")))'   || FALSE.apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:numbers".round 0 ("funcky:commons".error "foo")))' || FALSE.apply(engine)
        '"funcky:numbers".round 12.34 0 "funcky:numbers".HALF_UP'                                                  || new FunckyNumber(engine, 12.0G)
        '"funcky:numbers".round 12.34 1 "funcky:numbers".HALF_UP'                                                  || new FunckyNumber(engine, 12.3G)
        '"funcky:numbers".round 12.34 -1 "funcky:numbers".HALF_UP'                                                 || new FunckyNumber(engine, 10.0G)
        '"funcky:numbers".round 0.1 0 "funcky:numbers".UP'                                                         || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".round 0.1 0 "funcky:numbers".DOWN'                                                       || new FunckyNumber(engine, 0.0G)
    }

    @Unroll('Test round (runtime error, expression: #expression)')
    def 'Test round (runtime error)'(final String expression, final String message) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(message)
        where:
        expression                                                  || message
        '"funcky:numbers".round 0 0.1 "funcky:numbers".UP'          || String.format(Numbers.ERROR_INVALID_SCALE, 0.1G)
        '"funcky:numbers".round 0.1 0 0.1'                          || String.format(Numbers.ERROR_INVALID_ROUNDING_MODE, 0.1G)
        '"funcky:numbers".round 0.1 0 -1'                           || String.format(Numbers.ERROR_INVALID_ROUNDING_MODE, -1)
        '"funcky:numbers".round 0.1 0 "funcky:numbers".UNNECESSARY' || String.format(Numbers.ERROR_INSUFFICIENT_SCALE, 0, RoundingMode.UNNECESSARY.ordinal())
    }

    @Unroll('Test add (expression: #expression)')
    def 'Test add'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                      || result
        '"funcky:numbers".add'                                                          || new Numbers(engine).$add
        '"funcky:types".type "funcky:numbers".add'                                      || FUNCTION(NUMBER, NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:numbers".add 1)'                                  || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:commons".string ("funcky:numbers".add ("funcky:commons".error "foo"))' || engine.toFuncky('"funcky:numbers".add ("funcky:commons".error "foo")')
        '"funcky:numbers".add 0 0'                                                      || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".add 0 1'                                                      || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".add 1 1'                                                      || new FunckyNumber(engine, 2.0G)
        '"funcky:numbers".add 1 -1'                                                     || new FunckyNumber(engine, 0.0G)
    }

    @Unroll('Test subtract (expression: #expression)')
    def 'Test subtract'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                           || result
        '"funcky:numbers".subtract'                                                          || new Numbers(engine).$subtract
        '"funcky:types".type "funcky:numbers".subtract'                                      || FUNCTION(NUMBER, NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:numbers".subtract 1)'                                  || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:commons".string ("funcky:numbers".subtract ("funcky:commons".error "foo"))' || engine.toFuncky('"funcky:numbers".subtract ("funcky:commons".error "foo")')
        '"funcky:numbers".subtract 0 0'                                                      || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".subtract 0 1'                                                      || new FunckyNumber(engine, -1.0G)
        '"funcky:numbers".subtract 1 1'                                                      || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".subtract 1 -1'                                                     || new FunckyNumber(engine, 2.0G)
    }

    @Unroll('Test multiply (expression: #expression)')
    def 'Test multiply'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                           || result
        '"funcky:numbers".multiply'                                                          || new Numbers(engine).$multiply
        '"funcky:types".type "funcky:numbers".multiply'                                      || FUNCTION(NUMBER, NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:numbers".multiply 1)'                                  || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:commons".string ("funcky:numbers".multiply ("funcky:commons".error "foo"))' || engine.toFuncky('"funcky:numbers".multiply ("funcky:commons".error "foo")')
        '"funcky:numbers".multiply 0 0'                                                      || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".multiply 0 1'                                                      || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".multiply 1 1'                                                      || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".multiply 1 -1'                                                     || new FunckyNumber(engine, -1.0G)
    }

    @Unroll('Test divide (expression: #expression)')
    def 'Test divide'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                             || result
        '"funcky:numbers".divide'                                                              || new Numbers(engine).$divide
        '"funcky:types".type "funcky:numbers".divide'                                          || FUNCTION(NUMBER, NUMBER, NUMBER, NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:numbers".divide 1)'                                      || FUNCTION(NUMBER, NUMBER, NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:numbers".divide 1 2)'                                    || FUNCTION(NUMBER, NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:numbers".divide 1 2 3)'                                  || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:commons".string ("funcky:numbers".divide ("funcky:commons".error "foo"))'     || engine.toFuncky('"funcky:numbers".divide ("funcky:commons".error "foo")')
        '"funcky:commons".string ("funcky:numbers".divide 1 ("funcky:commons".error "foo"))'   || engine.toFuncky('"funcky:numbers".divide 1 ("funcky:commons".error "foo")')
        '"funcky:commons".string ("funcky:numbers".divide 1 2 ("funcky:commons".error "foo"))' || engine.toFuncky('"funcky:numbers".divide 1 2 ("funcky:commons".error "foo")')
        '"funcky:numbers".divide 1 3 4 "funcky:numbers".UP'                                    || new FunckyNumber(engine, 0.3334G)
        '"funcky:numbers".divide 123 4 -1 "funcky:numbers".UP'                                 || new FunckyNumber(engine, 40.0G)
        '"funcky:numbers".divide 1 3 2 "funcky:numbers".UP'                                    || new FunckyNumber(engine, 0.34G)
        '"funcky:numbers".divide 1 3 2 "funcky:numbers".DOWN'                                  || new FunckyNumber(engine, 0.33G)
    }

    @Unroll('Test divide (runtime error, expression: #expression)')
    def 'Test divide (runtime error)'(final String expression, final String message) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(message)
        where:
        expression                                                   || message
        '"funcky:numbers".divide 1 0 0 "funcky:numbers".UP'          || Numbers.ERROR_DIVISION_BY_ZERO
        '"funcky:numbers".divide 1 2 0.1 "funcky:numbers".UP'        || String.format(Numbers.ERROR_INVALID_SCALE, 0.1G)
        '"funcky:numbers".divide 1 2 0 0.1'                          || String.format(Numbers.ERROR_INVALID_ROUNDING_MODE, 0.1G)
        '"funcky:numbers".divide 1 2 0 -1'                           || String.format(Numbers.ERROR_INVALID_ROUNDING_MODE, -1)
        '"funcky:numbers".divide 1 3 0 "funcky:numbers".UNNECESSARY' || String.format(Numbers.ERROR_INSUFFICIENT_SCALE, 0, RoundingMode.UNNECESSARY.ordinal())
    }

    @Unroll('Test modulo (expression: #expression)')
    def 'Test modulo'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:numbers".modulo)'                                  || FALSE.apply(engine)
        '"funcky:types".type "funcky:numbers".modulo'                                                             || FUNCTION(NUMBER, NUMBER, NUMBER).apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:numbers".modulo ("funcky:commons".error "foo")))' || FALSE.apply(engine)
        '"funcky:numbers".modulo 7 3'                                                                             || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".modulo 7 -3'                                                                            || new FunckyNumber(engine, -2.0G)
        '"funcky:numbers".modulo -7 3'                                                                            || new FunckyNumber(engine, 2.0G)
        '"funcky:numbers".modulo -7 -3'                                                                           || new FunckyNumber(engine, -1.0G)
    }

    @Unroll('Test modulo (runtime error, expression: #expression)')
    def 'Test modulo (runtime error)'(final String expression, final String message) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(message)
        where:
        expression                    || message
        '"funcky:numbers".modulo 1 0' || Numbers.ERROR_DIVISION_BY_ZERO
    }

    @Unroll('Test byte (expression: #expression)')
    def 'Test byte'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                  || result
        '"funcky:numbers".byte'                     || new Numbers(engine).$byte
        '"funcky:types".type "funcky:numbers".byte' || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:numbers".byte 0'                   || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".byte 1'                   || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".byte 127'                 || new FunckyNumber(engine, 127.0G)
        '"funcky:numbers".byte -128'                || new FunckyNumber(engine, -128.0G)
        '"funcky:numbers".byte 0.1'                 || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".byte 0.9'                 || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".byte 1.1'                 || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".byte 128'                 || new FunckyNumber(engine, -128.0G)
        '"funcky:numbers".byte 129'                 || new FunckyNumber(engine, -127.0G)
        '"funcky:numbers".byte -129'                || new FunckyNumber(engine, 127.0G)
        '"funcky:numbers".byte -130'                || new FunckyNumber(engine, 126.0G)
    }

    @Unroll('Test short (expression: #expression)')
    def 'Test short'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                   || result
        '"funcky:numbers".short'                     || new Numbers(engine).$short
        '"funcky:types".type "funcky:numbers".short' || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:numbers".short 0'                   || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".short 1'                   || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".short 32767'               || new FunckyNumber(engine, 32767.0G)
        '"funcky:numbers".short -32768'              || new FunckyNumber(engine, -32768.0G)
        '"funcky:numbers".short 0.1'                 || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".short 0.9'                 || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".short 1.1'                 || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".short 32768'               || new FunckyNumber(engine, -32768.0G)
        '"funcky:numbers".short 32769'               || new FunckyNumber(engine, -32767.0G)
        '"funcky:numbers".short -32769'              || new FunckyNumber(engine, 32767.0G)
        '"funcky:numbers".short -32770'              || new FunckyNumber(engine, 32766.0G)
    }

    @Unroll('Test int (expression: #expression)')
    def 'Test int'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                 || result
        '"funcky:numbers".int'                     || new Numbers(engine).$int
        '"funcky:types".type "funcky:numbers".int' || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:numbers".int 0'                   || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".int 1'                   || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".int 2147483647'          || new FunckyNumber(engine, 2147483647.0G)
        '"funcky:numbers".int -2147483648'         || new FunckyNumber(engine, -2147483648.0G)
        '"funcky:numbers".int 0.1'                 || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".int 0.9'                 || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".int 1.1'                 || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".int 2147483648'          || new FunckyNumber(engine, -2147483648.0G)
        '"funcky:numbers".int 2147483649'          || new FunckyNumber(engine, -2147483647.0G)
        '"funcky:numbers".int -2147483649'         || new FunckyNumber(engine, 2147483647.0G)
        '"funcky:numbers".int -2147483650'         || new FunckyNumber(engine, 2147483646.0G)
    }

    @Unroll('Test long (expression: #expression)')
    def 'Test long'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                   || result
        '"funcky:numbers".long'                      || new Numbers(engine).$long
        '"funcky:types".type "funcky:numbers".long'  || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:numbers".long 0'                    || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".long 1'                    || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".long 9223372036854775807'  || new FunckyNumber(engine, 9223372036854775807.0G)
        '"funcky:numbers".long -9223372036854775808' || new FunckyNumber(engine, -9223372036854775808.0G)
        '"funcky:numbers".long 0.1'                  || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".long 0.9'                  || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".long 1.1'                  || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".long 9223372036854775808'  || new FunckyNumber(engine, -9223372036854775808.0G)
        '"funcky:numbers".long 9223372036854775809'  || new FunckyNumber(engine, -9223372036854775807.0G)
        '"funcky:numbers".long -9223372036854775809' || new FunckyNumber(engine, 9223372036854775807.0G)
        '"funcky:numbers".long -9223372036854775810' || new FunckyNumber(engine, 9223372036854775806.0G)
    }

    @Unroll('Test float (expression: #expression)')
    def 'Test float'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                   || result
        '"funcky:numbers".float'                     || new Numbers(engine).$float
        '"funcky:types".type "funcky:numbers".float' || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:numbers".float 0'                   || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".float 1'                   || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".float -1'                  || new FunckyNumber(engine, -1.0G)
        '"funcky:numbers".float 0.1'                 || new FunckyNumber(engine, 0.100000001490116119384765625G)
    }

    @Unroll('Test double (expression: #expression)')
    def 'Test double'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:numbers".double'                     || new Numbers(engine).$double
        '"funcky:types".type "funcky:numbers".double' || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:numbers".double 0'                   || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".double 1'                   || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".double -1'                  || new FunckyNumber(engine, -1.0G)
        '"funcky:numbers".double 0.1'                 || new FunckyNumber(engine, 0.1000000000000000055511151231257827021181583404541015625G)
    }

    @Unroll('Test sum (expression: #expression)')
    def 'Test sum'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                            || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:numbers".sum)' || FALSE.apply(engine)
        '"funcky:types".type "funcky:numbers".sum'                            || FUNCTION(LIST(NUMBER), NUMBER).apply(engine)
        '"funcky:numbers".sum []'                                             || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".sum [0]'                                            || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".sum [1]'                                            || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".sum [0, 1]'                                         || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".sum [1, 2]'                                         || new FunckyNumber(engine, 3.0G)
        '"funcky:numbers".sum [2, 3]'                                         || new FunckyNumber(engine, 5.0G)
    }

    @Unroll('Test product (expression: #expression)')
    def 'Test product'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:numbers".product)' || FALSE.apply(engine)
        '"funcky:types".type "funcky:numbers".product'                            || FUNCTION(LIST(NUMBER), NUMBER).apply(engine)
        '"funcky:numbers".product []'                                             || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".product [0]'                                            || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".product [1]'                                            || new FunckyNumber(engine, 1.0G)
        '"funcky:numbers".product [0, 1]'                                         || new FunckyNumber(engine, 0.0G)
        '"funcky:numbers".product [1, 2]'                                         || new FunckyNumber(engine, 2.0G)
        '"funcky:numbers".product [2, 3]'                                         || new FunckyNumber(engine, 6.0G)
    }
}
