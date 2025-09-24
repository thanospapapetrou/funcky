package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType
import io.github.thanospapapetrou.funcky.runtime.FunckyListType
import spock.lang.Unroll

class BooleansSpec extends BaseSpec {
    @Unroll('Test false (expression: #expression)')
    def 'Test false'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:booleans".false'                     || $false
        '"funcky:types".type "funcky:booleans".false' || $Boolean
    }

    @Unroll('Test true (expression: #expression)')
    def 'Test true'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                   || result
        '"funcky:booleans".true'                     || $true
        '"funcky:types".type "funcky:booleans".true' || $Boolean
    }

    @Unroll('Test not (expression: #expression)')
    def 'Test not'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                             || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".not)' || $false
        '"funcky:types".type "funcky:booleans".not'                            || new FunckyFunctionType(engine, $Boolean, $Boolean)
        '"funcky:booleans".not "funcky:booleans".false'                        || $true
        '"funcky:booleans".not "funcky:booleans".true'                         || $false
    }

    @Unroll('Test and (expression: #expression)')
    def 'Test and'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".and)'                                  || $false
        '"funcky:types".type "funcky:booleans".and'                                                             || new FunckyFunctionType(engine, $Boolean, $Boolean, $Boolean)
        '"funcky:types".type ("funcky:booleans".and "funcky:booleans".false)'                                   || new FunckyFunctionType(engine, $Boolean, $Boolean)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:booleans".and ("funcky:commons".error "foo")))' || $false
        '"funcky:booleans".and "funcky:booleans".false "funcky:booleans".false'                                 || $false
        '"funcky:booleans".and "funcky:booleans".false "funcky:booleans".true'                                  || $false
        '"funcky:booleans".and "funcky:booleans".true "funcky:booleans".false'                                  || $false
        '"funcky:booleans".and "funcky:booleans".true "funcky:booleans".true'                                   || $true
        '"funcky:booleans".and "funcky:booleans".false ("funcky:commons".error "foo")'                          || $false
    }

    @Unroll('Test or (expression: #expression)')
    def 'Test or'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                             || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".or)'                                  || $false
        '"funcky:types".type "funcky:booleans".or'                                                             || new FunckyFunctionType(engine, $Boolean, $Boolean, $Boolean)
        '"funcky:types".type ("funcky:booleans".or "funcky:booleans".false)'                                   || new FunckyFunctionType(engine, $Boolean, $Boolean)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:booleans".or ("funcky:commons".error "foo")))' || $false
        '"funcky:booleans".or "funcky:booleans".false "funcky:booleans".false'                                 || $false
        '"funcky:booleans".or "funcky:booleans".false "funcky:booleans".true'                                  || $true
        '"funcky:booleans".or "funcky:booleans".true "funcky:booleans".false'                                  || $true
        '"funcky:booleans".or "funcky:booleans".true "funcky:booleans".true'                                   || $true
        '"funcky:booleans".or "funcky:booleans".true ("funcky:commons".error "foo")'                           || $true
    }

    @Unroll('Test xor (expression: #expression)')
    def 'Test xor'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".xor)'                                  || $false
        '"funcky:types".type "funcky:booleans".xor'                                                             || new FunckyFunctionType(engine, $Boolean, $Boolean, $Boolean)
        '"funcky:types".type ("funcky:booleans".xor "funcky:booleans".false)'                                   || new FunckyFunctionType(engine, $Boolean, $Boolean)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:booleans".xor ("funcky:commons".error "foo")))' || $false
        '"funcky:booleans".xor "funcky:booleans".false "funcky:booleans".false'                                 || $false
        '"funcky:booleans".xor "funcky:booleans".false "funcky:booleans".true'                                  || $true
        '"funcky:booleans".xor "funcky:booleans".true "funcky:booleans".false'                                  || $true
        '"funcky:booleans".xor "funcky:booleans".true "funcky:booleans".true'                                   || $false
    }

    @Unroll('Test all (expression: #expression)')
    def 'Test all'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                 || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".all)'     || $false
        '"funcky:types".type "funcky:booleans".all'                                || new FunckyFunctionType(engine, new FunckyListType(engine, $Boolean), $Boolean)
        '"funcky:booleans".all []'                                                 || $true
        '"funcky:booleans".all ["funcky:booleans".false]'                          || $false
        '"funcky:booleans".all ["funcky:booleans".true]'                           || $true
        '"funcky:booleans".all ["funcky:booleans".false, "funcky:booleans".false]' || $false
        '"funcky:booleans".all ["funcky:booleans".false, "funcky:booleans".true]'  || $false
        '"funcky:booleans".all ["funcky:booleans".true, "funcky:booleans".false]'  || $false
        '"funcky:booleans".all ["funcky:booleans".true, "funcky:booleans".true]'   || $true
    }

    @Unroll('Test any (expression: #expression)')
    def 'Test any'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                 || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".any)'     || $false
        '"funcky:types".type "funcky:booleans".any'                                || new FunckyFunctionType(engine, new FunckyListType(engine, $Boolean), $Boolean)
        '"funcky:booleans".any []'                                                 || $false
        '"funcky:booleans".any ["funcky:booleans".false]'                          || $false
        '"funcky:booleans".any ["funcky:booleans".true]'                           || $true
        '"funcky:booleans".any ["funcky:booleans".false, "funcky:booleans".false]' || $false
        '"funcky:booleans".any ["funcky:booleans".false, "funcky:booleans".true]'  || $true
        '"funcky:booleans".any ["funcky:booleans".true, "funcky:booleans".false]'  || $true
        '"funcky:booleans".any ["funcky:booleans".true, "funcky:booleans".true]'   || $true
    }
}
