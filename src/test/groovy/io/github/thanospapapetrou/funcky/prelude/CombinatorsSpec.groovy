package io.github.thanospapapetrou.funcky.prelude


import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.prelude.Combinators
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.FALSE
import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.TRUE
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.BOOLEAN
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.TYPE

class CombinatorsSpec extends BaseSpec {
    @Unroll('Test s (expression: #expression)')
    def 'Test s'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                      || result
        '"funcky:commons".string "funcky:combinators".s'                                                                                                                                                                                                              || toFuncky('"funcky:combinators".s')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))'                                                                                                                                      || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                                                                                                               || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                                                                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                               || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                                 || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                       || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                        || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))) ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))' || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'   || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                       || FALSE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                        || FALSE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))' || FALSE.apply(engine)
        '"funcky:types".type ("funcky:combinators".s "funcky:numbers".add)'                                                                                                                                                                                             || FUNCTION(FUNCTION(NUMBER, NUMBER), NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:combinators".s "funcky:booleans".and)'                                                                                                                                                                                            || FUNCTION(FUNCTION(BOOLEAN, BOOLEAN), BOOLEAN, BOOLEAN).apply(engine)
        '"funcky:commons".string ("funcky:combinators".s ("funcky:commons".error "foo"))'                                                                                                                                                                               || toFuncky('"funcky:combinators".s ("funcky:commons".error "foo")')
        '"funcky:types".type ("funcky:combinators".s "funcky:numbers".add "funcky:numbers".plus)'                                                                                                                                                                       || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:combinators".s "funcky:booleans".and "funcky:booleans".not)'                                                                                                                                                                      || FUNCTION(BOOLEAN, BOOLEAN).apply(engine)
        '"funcky:commons".string ("funcky:combinators".s "funcky:numbers".add ("funcky:commons".error "foo"))'                                                                                                                                                          || toFuncky('"funcky:combinators".s "funcky:numbers".add ("funcky:commons".error "foo")')
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".plus 1'                                                                                                                                                                                           || new FunckyNumber(engine, 2.0G)
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".plus 2'                                                                                                                                                                                           || new FunckyNumber(engine, 4.0G)
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".minus 1'                                                                                                                                                                                          || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".minus 2'                                                                                                                                                                                          || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".plus 1'                                                                                                                                                                                      || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".plus 2'                                                                                                                                                                                      || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".minus 1'                                                                                                                                                                                     || new FunckyNumber(engine, 2.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".minus 2'                                                                                                                                                                                     || new FunckyNumber(engine, 4.0G)
        '"funcky:combinators".s "funcky:booleans".and "funcky:booleans".not "funcky:booleans".false'                                                                                                                                                                    || FALSE.apply(engine)
        '"funcky:combinators".s "funcky:booleans".and "funcky:booleans".not "funcky:booleans".true'                                                                                                                                                                     || FALSE.apply(engine)
        '"funcky:combinators".s "funcky:booleans".or "funcky:booleans".not "funcky:booleans".false'                                                                                                                                                                     || TRUE.apply(engine)
        '"funcky:combinators".s "funcky:booleans".or "funcky:booleans".not "funcky:booleans".true'                                                                                                                                                                      || TRUE.apply(engine)
    }

    @Unroll('Test k (expression: #expression)')
    def 'Test k'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                || result
        '"funcky:commons".string "funcky:combinators".k'                                                                                                                                          || toFuncky('"funcky:combinators".k')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:combinators".k))'                                                                                        || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))'                                                                 || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))'                                                                  || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:combinators".k)) ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))'  || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:combinators".k)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))' || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:combinators".k 0)))'                                                                                    || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".type ("funcky:combinators".k 0))'                                                                                                                   || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:combinators".k \'a\')))'                                                                                || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".type ("funcky:combinators".k \'a\'))'                                                                                                               || CHARACTER.apply(engine)
        '"funcky:commons".string ("funcky:combinators".k ("funcky:commons".error "foo"))'                                                                                                         || toFuncky('"funcky:combinators".k ("funcky:commons".error "foo")')
        '"funcky:combinators".k 0 \'a\''                                                                                                                                                          || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".k \'a\' 0'                                                                                                                                                          || new FunckyCharacter(engine, 'a' as char)
        '"funcky:combinators".k 0 ("funcky:commons".error "foo")'                                                                                                                                 || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".k \'a\' ("funcky:commons".error "foo")'                                                                                                                             || new FunckyCharacter(engine, 'a' as char)
    }

    @Unroll('Test i (expression: #expression)')
    def 'Test i'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                        || result
        '"funcky:commons".string ("funcky:combinators".i)'                                                                                                                || toFuncky('"funcky:combinators".s "funcky:combinators".k "funcky:combinators".k')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:combinators".i))'                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:combinators".i))'                                                                 || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:combinators".i)) ("funcky:types".range ("funcky:types".type "funcky:combinators".i))' || TRUE.apply(engine)
        '"funcky:combinators".i 0'                                                                                                                                        || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".i 1'                                                                                                                                        || new FunckyNumber(engine, 1.0G)
        '"funcky:combinators".i \'a\''                                                                                                                                    || new FunckyCharacter(engine, 'a' as char)
        '"funcky:combinators".i \'b\''                                                                                                                                    || new FunckyCharacter(engine, 'b' as char)
    }

    @Unroll('Test b (expression: #expression)')
    def 'Test b'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                      || result
        '"funcky:commons".string "funcky:combinators".b'                                                                                                                                                                                                                || toFuncky('"funcky:combinators".s ("funcky:combinators".k "funcky:combinators".s) "funcky:combinators".k')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b)))'                                                                                                                                      || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b)))'                                                                                                                                       || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                               || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                                 || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                        || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                          || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b)))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))' || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b)))'                                               || FALSE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                       || FALSE.apply(engine)
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                        || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus))))'                                                                                                             || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus)))'                                                                                                                                            || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus))))'                                                                                                              || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus)))'                                                                                                                                             || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not))))'                                                                                                              || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not)))'                                                                                                                                             || BOOLEAN.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not))))'                                                                                                               || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not)))'                                                                                                                                              || BOOLEAN.apply(engine)
        '"funcky:types".type ("funcky:combinators".b "funcky:numbers".minus "funcky:numbers".minus)'                                                                                                                                                                    || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:combinators".b "funcky:booleans".not "funcky:booleans".not)'                                                                                                                                                                      || FUNCTION(BOOLEAN, BOOLEAN).apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".b ("funcky:commons".error "foo")))'                                                                                                                                                        || FALSE.apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".b "funcky:numbers".minus ("funcky:commons".error "foo")))'                                                                                                                                 || FALSE.apply(engine)
        '"funcky:combinators".b ("funcky:numbers".add 1) "funcky:numbers".minus 2'                                                                                                                                                                                      || new FunckyNumber(engine, -1.0G)
        '"funcky:combinators".b ("funcky:types".Function "funcky:types".Type) "funcky:types".type 1'                                                                                                                                                                    || FUNCTION(TYPE, NUMBER).apply(engine)
    }

    @Unroll('Test c (expression: #expression)')
    def 'Test c'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                      || result
        '"funcky:commons".string "funcky:combinators".c'                                                                                                                                                                                                                || toFuncky('"funcky:combinators".s ("funcky:combinators".s ("funcky:combinators".k ("funcky:combinators".s ("funcky:combinators".k "funcky:combinators".s) "funcky:combinators".k)) "funcky:combinators".s) ("funcky:combinators".k "funcky:combinators".k)')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))'                                                                                                                                      || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                                                                                                               || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                                                                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".c)))'                                                                                                                                       || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'                                                                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'                                                                                                                 || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'                        || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".c)))'                        || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'   || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                       || FALSE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                        || FALSE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))' || FALSE.apply(engine)
        '"funcky:types".type ("funcky:combinators".c "funcky:numbers".add)'                                                                                                                                                                                             || FUNCTION(NUMBER, NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:combinators".c "funcky:booleans".and)'                                                                                                                                                                                            || FUNCTION(BOOLEAN, BOOLEAN, BOOLEAN).apply(engine)
        '"funcky:types".type ("funcky:combinators".c "funcky:numbers".add 0)'                                                                                                                                                                                           || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:combinators".c "funcky:booleans".and "funcky:booleans".false)'                                                                                                                                                                    || FUNCTION(BOOLEAN, BOOLEAN).apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".c ("funcky:commons".error "foo")))'                                                                                                                                                        || FALSE.apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".c "funcky:numbers".add ("funcky:commons".error "foo")))'                                                                                                                                   || FALSE.apply(engine)
        '"funcky:combinators".c "funcky:numbers".subtract 1 3'                                                                                                                                                                                                          || new FunckyNumber(engine, 2.0G)
        '"funcky:combinators".c "funcky:types".Function "funcky:types".Type "funcky:types".Number'                                                                                                                                                                      || FUNCTION(NUMBER, TYPE).apply(engine)
    }

    @Unroll('Test w (expression: #expression)')
    def 'Test w'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                || result
        '"funcky:commons".string "funcky:combinators".w'                                                                                                                                                                                          || toFuncky('"funcky:combinators".s "funcky:combinators".s ("funcky:combinators".s "funcky:combinators".k)')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w)))'                                                                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))'                                                                                         || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))'                                                                                          || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'                                                                                                                 || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'                                                                                                                  || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))' || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'                         || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w)))) ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'    || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))'  || FALSE.apply(engine)
        '"funcky:types".type ("funcky:combinators".w "funcky:numbers".add)'                                                                                                                                                                       || FUNCTION(NUMBER, NUMBER).apply(engine)
        '"funcky:types".type ("funcky:combinators".w "funcky:booleans".and)'                                                                                                                                                                      || FUNCTION(BOOLEAN, BOOLEAN).apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".w ("funcky:commons".error "foo")))'                                                                                                                                  || FALSE.apply(engine)
        '"funcky:combinators".w "funcky:numbers".add 1'                                                                                                                                                                                           || new FunckyNumber(engine, 2.0G)
        '"funcky:combinators".w "funcky:numbers".multiply 2'                                                                                                                                                                                      || new FunckyNumber(engine, 4.0G)
        '"funcky:combinators".w "funcky:booleans".and "funcky:booleans".true'                                                                                                                                                                     || TRUE.apply(engine)
    }
}
