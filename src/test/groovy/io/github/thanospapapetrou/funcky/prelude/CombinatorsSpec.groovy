package io.github.thanospapapetrou.funcky.prelude


import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.FunckyJavaConverter
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.prelude.Combinators
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.FALSE
import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.TRUE
import static io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType.FUNCTION
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.BOOLEAN
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.CHARACTER
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.NUMBER
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.TYPE

class CombinatorsSpec extends BaseSpec {
    @Unroll('Test s (expression: #expression)')
    def 'Test s'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                      || result
        '"funcky:combinators".s'                                                                                                                                                                                                                                        || new Combinators(engine).$s
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))'                                                                                                                                      || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                                                                                                               || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                                                                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                               || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                                 || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                       || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                        || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))) ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))' || TRUE
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'   || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                       || FALSE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                        || FALSE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))' || FALSE
        '"funcky:types".type ("funcky:combinators".s "funcky:numbers".add)'                                                                                                                                                                                             || FUNCTION(FUNCTION(NUMBER, NUMBER), NUMBER, NUMBER)
        '"funcky:types".type ("funcky:combinators".s "funcky:booleans".and)'                                                                                                                                                                                            || FUNCTION(FUNCTION(BOOLEAN, BOOLEAN), BOOLEAN, BOOLEAN)
        '"funcky:commons".string ("funcky:combinators".s ("funcky:commons".error "foo"))'                                                                                                                                                                               || new FunckyJavaConverter().convert('"funcky:combinators".s ("funcky:commons".error "foo")')
        '"funcky:types".type ("funcky:combinators".s "funcky:numbers".add "funcky:numbers".plus)'                                                                                                                                                                       || FUNCTION(NUMBER, NUMBER)
        '"funcky:types".type ("funcky:combinators".s "funcky:booleans".and "funcky:booleans".not)'                                                                                                                                                                      || FUNCTION(BOOLEAN, BOOLEAN)
        '"funcky:commons".string ("funcky:combinators".s "funcky:numbers".add ("funcky:commons".error "foo"))'                                                                                                                                                          || new FunckyJavaConverter().convert('"funcky:combinators".s "funcky:numbers".add ("funcky:commons".error "foo")')
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".plus 1'                                                                                                                                                                                           || new FunckyNumber(2.0G)
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".plus 2'                                                                                                                                                                                           || new FunckyNumber(4.0G)
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".minus 1'                                                                                                                                                                                          || new FunckyNumber(0.0G)
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".minus 2'                                                                                                                                                                                          || new FunckyNumber(0.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".plus 1'                                                                                                                                                                                      || new FunckyNumber(0.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".plus 2'                                                                                                                                                                                      || new FunckyNumber(0.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".minus 1'                                                                                                                                                                                     || new FunckyNumber(2.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".minus 2'                                                                                                                                                                                     || new FunckyNumber(4.0G)
        '"funcky:combinators".s "funcky:booleans".and "funcky:booleans".not "funcky:booleans".false'                                                                                                                                                                    || FALSE
        '"funcky:combinators".s "funcky:booleans".and "funcky:booleans".not "funcky:booleans".true'                                                                                                                                                                     || FALSE
        '"funcky:combinators".s "funcky:booleans".or "funcky:booleans".not "funcky:booleans".false'                                                                                                                                                                     || TRUE
        '"funcky:combinators".s "funcky:booleans".or "funcky:booleans".not "funcky:booleans".true'                                                                                                                                                                      || TRUE
    }

    @Unroll('Test k (expression: #expression)')
    def 'Test k'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                || result
        '"funcky:combinators".k'                                                                                                                                                                  || new Combinators(engine).$k
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:combinators".k))'                                                                                        || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))'                                                                 || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))'                                                                  || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:combinators".k)) ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))'  || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:combinators".k)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))' || FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:combinators".k 0)))'                                                                                    || TRUE
        '"funcky:types".range ("funcky:types".type ("funcky:combinators".k 0))'                                                                                                                   || NUMBER
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:combinators".k \'a\')))'                                                                                || TRUE
        '"funcky:types".range ("funcky:types".type ("funcky:combinators".k \'a\'))'                                                                                                               || CHARACTER
        '"funcky:commons".string ("funcky:combinators".k ("funcky:commons".error "foo"))'                                                                                                         || new FunckyJavaConverter().convert('"funcky:combinators".k ("funcky:commons".error "foo")')
        '"funcky:combinators".k 0 \'a\''                                                                                                                                                          || new FunckyNumber(0.0G)
        '"funcky:combinators".k \'a\' 0'                                                                                                                                                          || new FunckyCharacter('a' as char)
        '"funcky:combinators".k 0 ("funcky:commons".error "foo")'                                                                                                                                 || new FunckyNumber(0.0G)
        '"funcky:combinators".k \'a\' ("funcky:commons".error "foo")'                                                                                                                             || new FunckyCharacter('a' as char)
    }

    @Unroll('Test i (expression: #expression)')
    def 'Test i'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                        || result
        '"funcky:commons".string ("funcky:combinators".i)'                                                                                                                || new FunckyJavaConverter().convert('"funcky:combinators".s "funcky:combinators".k "funcky:combinators".k')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:combinators".i))'                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:combinators".i))'                                                                 || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:combinators".i)) ("funcky:types".range ("funcky:types".type "funcky:combinators".i))' || TRUE
        '"funcky:combinators".i 0'                                                                                                                                        || new FunckyNumber(0.0G)
        '"funcky:combinators".i 1'                                                                                                                                        || new FunckyNumber(1.0G)
        '"funcky:combinators".i \'a\''                                                                                                                                    || new FunckyCharacter('a' as char)
        '"funcky:combinators".i \'b\''                                                                                                                                    || new FunckyCharacter('b' as char)
    }

    @Unroll('Test b (expression: #expression)')
    def 'Test b'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                      || result
        '"funcky:commons".string "funcky:combinators".b'                                                                                                                                                                                                                || new FunckyJavaConverter().convert('"funcky:combinators".s ("funcky:combinators".k "funcky:combinators".s) "funcky:combinators".k')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b)))'                                                                                                                                      || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b)))'                                                                                                                                       || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                               || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                                 || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                        || TRUE
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                          || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b)))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))' || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b)))'                                               || FALSE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                       || FALSE
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                        || FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus))))'                                                                                                             || TRUE
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus)))'                                                                                                                                            || NUMBER
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus))))'                                                                                                              || TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus)))'                                                                                                                                             || NUMBER
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not))))'                                                                                                              || TRUE
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not)))'                                                                                                                                             || BOOLEAN
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not))))'                                                                                                               || TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not)))'                                                                                                                                              || BOOLEAN
        '"funcky:types".type ("funcky:combinators".b "funcky:numbers".minus "funcky:numbers".minus)'                                                                                                                                                                    || FUNCTION(NUMBER, NUMBER)
        '"funcky:types".type ("funcky:combinators".b "funcky:booleans".not "funcky:booleans".not)'                                                                                                                                                                      || FUNCTION(BOOLEAN, BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".b ("funcky:commons".error "foo")))'                                                                                                                                                        || FALSE
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".b "funcky:numbers".minus ("funcky:commons".error "foo")))'                                                                                                                                 || FALSE
        '"funcky:combinators".b ("funcky:numbers".add 1) "funcky:numbers".minus 2'                                                                                                                                                                                      || new FunckyNumber(-1.0G)
        '"funcky:combinators".b ("funcky:types".Function "funcky:types".Type) "funcky:types".type 1'                                                                                                                                                                    || FUNCTION(TYPE, NUMBER)
    }

    @Unroll('Test c (expression: #expression)')
    def 'Test c'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                      || result
        '"funcky:commons".string "funcky:combinators".c'                                                                                                                                                                                                                || new FunckyJavaConverter().convert('"funcky:combinators".s ("funcky:combinators".s ("funcky:combinators".k ("funcky:combinators".s ("funcky:combinators".k "funcky:combinators".s) "funcky:combinators".k)) "funcky:combinators".s) ("funcky:combinators".k "funcky:combinators".k)')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))'                                                                                                                                      || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                                                                                                               || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                                                                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".c)))'                                                                                                                                       || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'                                                                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'                                                                                                                 || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'                        || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".c)))'                        || TRUE
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'   || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                       || FALSE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                        || FALSE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))' || FALSE
        '"funcky:types".type ("funcky:combinators".c "funcky:numbers".add)'                                                                                                                                                                                             || FUNCTION(NUMBER, NUMBER, NUMBER)
        '"funcky:types".type ("funcky:combinators".c "funcky:booleans".and)'                                                                                                                                                                                            || FUNCTION(BOOLEAN, BOOLEAN, BOOLEAN)
        '"funcky:types".type ("funcky:combinators".c "funcky:numbers".add 0)'                                                                                                                                                                                           || FUNCTION(NUMBER, NUMBER)
        '"funcky:types".type ("funcky:combinators".c "funcky:booleans".and "funcky:booleans".false)'                                                                                                                                                                    || FUNCTION(BOOLEAN, BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".c ("funcky:commons".error "foo")))'                                                                                                                                                        || FALSE
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".c "funcky:numbers".add ("funcky:commons".error "foo")))'                                                                                                                                   || FALSE
        '"funcky:combinators".c "funcky:numbers".subtract 1 3'                                                                                                                                                                                                          || new FunckyNumber(2.0G)
        '"funcky:combinators".c "funcky:types".Function "funcky:types".Type "funcky:types".Number'                                                                                                                                                                      || FUNCTION(NUMBER, TYPE)
    }

    @Unroll('Test w (expression: #expression)')
    def 'Test w'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                || result
        '"funcky:commons".string "funcky:combinators".w'                                                                                                                                                                                          || new FunckyJavaConverter().convert('"funcky:combinators".s "funcky:combinators".s ("funcky:combinators".s "funcky:combinators".k)')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w)))'                                                                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))'                                                                                         || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))'                                                                                          || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'                                                                                                                 || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'                                                                                                                  || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))' || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'                         || TRUE
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w)))) ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'    || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))'  || FALSE
        '"funcky:types".type ("funcky:combinators".w "funcky:numbers".add)'                                                                                                                                                                       || FUNCTION(NUMBER, NUMBER)
        '"funcky:types".type ("funcky:combinators".w "funcky:booleans".and)'                                                                                                                                                                      || FUNCTION(BOOLEAN, BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".w ("funcky:commons".error "foo")))'                                                                                                                                  || FALSE
        '"funcky:combinators".w "funcky:numbers".add 1'                                                                                                                                                                                           || new FunckyNumber(2.0G)
        '"funcky:combinators".w "funcky:numbers".multiply 2'                                                                                                                                                                                      || new FunckyNumber(4.0G)
        '"funcky:combinators".w "funcky:booleans".and "funcky:booleans".true'                                                                                                                                                                     || TRUE
    }
}
