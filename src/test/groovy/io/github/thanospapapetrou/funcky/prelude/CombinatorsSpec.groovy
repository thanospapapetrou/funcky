package io.github.thanospapapetrou.funcky.prelude


import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.prelude.Combinators
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType
import spock.lang.Unroll

class CombinatorsSpec extends BaseSpec {
    @Unroll('Test s (expression: #expression)')
    def 'Test s'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                      || result
        '"funcky:combinators".s'                                                                                                                                                                                                                                        || new Combinators(engine).$s
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))'                                                                                                                                      || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                                                                                                               || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                                                                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                               || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                                                                                                                 || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                       || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'                        || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))) ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))' || $true
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".s))))'   || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                       || $false
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))'                        || $false
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s)))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".s))))' || $false
        '"funcky:types".type ("funcky:combinators".s "funcky:numbers".add)'                                                                                                                                                                                             || new FunckyFunctionType(engine, new FunckyFunctionType(engine, $Number, $Number), $Number, $Number)
        '"funcky:types".type ("funcky:combinators".s "funcky:booleans".and)'                                                                                                                                                                                            || new FunckyFunctionType(engine, new FunckyFunctionType(engine, $Boolean, $Boolean), $Boolean, $Boolean)
        '"funcky:commons".string ("funcky:combinators".s ("funcky:commons".error "foo"))'                                                                                                                                                                               || engine.converter.convert('"funcky:combinators".s ("funcky:commons".error "foo")')
        '"funcky:types".type ("funcky:combinators".s "funcky:numbers".add "funcky:numbers".plus)'                                                                                                                                                                       || new FunckyFunctionType(engine, $Number, $Number)
        '"funcky:types".type ("funcky:combinators".s "funcky:booleans".and "funcky:booleans".not)'                                                                                                                                                                      || new FunckyFunctionType(engine, $Boolean, $Boolean)
        '"funcky:commons".string ("funcky:combinators".s "funcky:numbers".add ("funcky:commons".error "foo"))'                                                                                                                                                          || engine.converter.convert('"funcky:combinators".s "funcky:numbers".add ("funcky:commons".error "foo")')
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".plus 1'                                                                                                                                                                                           || new FunckyNumber(engine, 2.0G)
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".plus 2'                                                                                                                                                                                           || new FunckyNumber(engine, 4.0G)
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".minus 1'                                                                                                                                                                                          || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".s "funcky:numbers".add "funcky:numbers".minus 2'                                                                                                                                                                                          || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".plus 1'                                                                                                                                                                                      || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".plus 2'                                                                                                                                                                                      || new FunckyNumber(engine, 0.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".minus 1'                                                                                                                                                                                     || new FunckyNumber(engine, 2.0G)
        '"funcky:combinators".s "funcky:numbers".subtract "funcky:numbers".minus 2'                                                                                                                                                                                     || new FunckyNumber(engine, 4.0G)
        '"funcky:combinators".s "funcky:booleans".and "funcky:booleans".not "funcky:booleans".false'                                                                                                                                                                    || $false
        '"funcky:combinators".s "funcky:booleans".and "funcky:booleans".not "funcky:booleans".true'                                                                                                                                                                     || $false
        '"funcky:combinators".s "funcky:booleans".or "funcky:booleans".not "funcky:booleans".false'                                                                                                                                                                     || $true
        '"funcky:combinators".s "funcky:booleans".or "funcky:booleans".not "funcky:booleans".true'                                                                                                                                                                      || $true
    }

    @Unroll('Test k (expression: #expression)')
    def 'Test k'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                || result
        '"funcky:combinators".k'                                                                                                                                                                  || new Combinators(engine).$k
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:combinators".k))'                                                                                        || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))'                                                                 || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))'                                                                  || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:combinators".k)) ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))'  || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:combinators".k)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".k)))' || $false
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:combinators".k 0)))'                                                                                    || $true
        '"funcky:types".range ("funcky:types".type ("funcky:combinators".k 0))'                                                                                                                   || $Number
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:combinators".k \'a\')))'                                                                                || $true
        '"funcky:types".range ("funcky:types".type ("funcky:combinators".k \'a\'))'                                                                                                               || $Character
        '"funcky:commons".string ("funcky:combinators".k ("funcky:commons".error "foo"))'                                                                                                         || engine.converter.convert('"funcky:combinators".k ("funcky:commons".error "foo")')
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
        '"funcky:commons".string ("funcky:combinators".i)'                                                                                                                || engine.converter.convert('"funcky:combinators".s "funcky:combinators".k "funcky:combinators".k')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:combinators".i))'                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:combinators".i))'                                                                 || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:combinators".i)) ("funcky:types".range ("funcky:types".type "funcky:combinators".i))' || $true
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
        '"funcky:commons".string "funcky:combinators".b'                                                                                                                                                                                                                || engine.converter.convert('"funcky:combinators".s ("funcky:combinators".k "funcky:combinators".s) "funcky:combinators".k')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b)))'                                                                                                                                      || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b)))'                                                                                                                                       || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                               || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                                                                                                                 || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                        || $true
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                          || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b)))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))' || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b)))'                                               || $false
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                       || $false
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".b))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".b))))'                        || $false
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus))))'                                                                                                             || $true
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus)))'                                                                                                                                            || $Number
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus))))'                                                                                                              || $true
        '"funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:numbers".minus)))'                                                                                                                                             || $Number
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not))))'                                                                                                              || $true
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not)))'                                                                                                                                             || $Boolean
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not))))'                                                                                                               || $true
        '"funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:combinators".b "funcky:booleans".not)))'                                                                                                                                              || $Boolean
        '"funcky:types".type ("funcky:combinators".b "funcky:numbers".minus "funcky:numbers".minus)'                                                                                                                                                                    || new FunckyFunctionType(engine, $Number, $Number)
        '"funcky:types".type ("funcky:combinators".b "funcky:booleans".not "funcky:booleans".not)'                                                                                                                                                                      || new FunckyFunctionType(engine, $Boolean, $Boolean)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".b ("funcky:commons".error "foo")))'                                                                                                                                                        || $false
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".b "funcky:numbers".minus ("funcky:commons".error "foo")))'                                                                                                                                 || $false
        '"funcky:combinators".b ("funcky:numbers".add 1) "funcky:numbers".minus 2'                                                                                                                                                                                      || new FunckyNumber(engine, -1.0G)
        '"funcky:combinators".b ("funcky:types".Function "funcky:types".Type) "funcky:types".type 1'                                                                                                                                                                    || new FunckyFunctionType(engine, $Type, $Number)
    }

    @Unroll('Test c (expression: #expression)')
    def 'Test c'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                      || result
        '"funcky:commons".string "funcky:combinators".c'                                                                                                                                                                                                                || engine.converter.convert('"funcky:combinators".s ("funcky:combinators".s ("funcky:combinators".k ("funcky:combinators".s ("funcky:combinators".k "funcky:combinators".s) "funcky:combinators".k)) "funcky:combinators".s) ("funcky:combinators".k "funcky:combinators".k)')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))'                                                                                                                                      || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                                                                                                               || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                                                                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".c)))'                                                                                                                                       || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'                                                                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'                                                                                                                 || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'                        || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".c)))'                        || $true
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".c))))'   || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                       || $false
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))'                        || $false
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c)))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".c))))' || $false
        '"funcky:types".type ("funcky:combinators".c "funcky:numbers".add)'                                                                                                                                                                                             || new FunckyFunctionType(engine, $Number, $Number, $Number)
        '"funcky:types".type ("funcky:combinators".c "funcky:booleans".and)'                                                                                                                                                                                            || new FunckyFunctionType(engine, $Boolean, $Boolean, $Boolean)
        '"funcky:types".type ("funcky:combinators".c "funcky:numbers".add 0)'                                                                                                                                                                                           || new FunckyFunctionType(engine, $Number, $Number)
        '"funcky:types".type ("funcky:combinators".c "funcky:booleans".and "funcky:booleans".false)'                                                                                                                                                                    || new FunckyFunctionType(engine, $Boolean, $Boolean)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".c ("funcky:commons".error "foo")))'                                                                                                                                                        || $false
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".c "funcky:numbers".add ("funcky:commons".error "foo")))'                                                                                                                                   || $false
        '"funcky:combinators".c "funcky:numbers".subtract 1 3'                                                                                                                                                                                                          || new FunckyNumber(engine, 2.0G)
        '"funcky:combinators".c "funcky:types".Function "funcky:types".Type "funcky:types".Number'                                                                                                                                                                      || new FunckyFunctionType(engine, $Number, $Type)
    }

    @Unroll('Test w (expression: #expression)')
    def 'Test w'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                || result
        '"funcky:commons".string "funcky:combinators".w'                                                                                                                                                                                          || engine.converter.convert('"funcky:combinators".s "funcky:combinators".s ("funcky:combinators".s "funcky:combinators".k)')
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w)))'                                                                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))'                                                                                         || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))'                                                                                          || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'                                                                                                                 || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'                                                                                                                  || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))' || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'                         || $true
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w)))) ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:combinators".w)))'    || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type "funcky:combinators".w))))'  || $false
        '"funcky:types".type ("funcky:combinators".w "funcky:numbers".add)'                                                                                                                                                                       || new FunckyFunctionType(engine, $Number, $Number)
        '"funcky:types".type ("funcky:combinators".w "funcky:booleans".and)'                                                                                                                                                                      || new FunckyFunctionType(engine, $Boolean, $Boolean)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:combinators".w ("funcky:commons".error "foo")))'                                                                                                                                  || $false
        '"funcky:combinators".w "funcky:numbers".add 1'                                                                                                                                                                                           || new FunckyNumber(engine, 2.0G)
        '"funcky:combinators".w "funcky:numbers".multiply 2'                                                                                                                                                                                      || new FunckyNumber(engine, 4.0G)
        '"funcky:combinators".w "funcky:booleans".and "funcky:booleans".true'                                                                                                                                                                     || $true
    }
}
