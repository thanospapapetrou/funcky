package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.FALSE
import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.TRUE
import static io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType.FUNCTION
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.LIST
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.BOOLEAN

class BooleansSpec extends BaseSpec {
    @Unroll('Test false (expression: #expression)')
    def 'Test false'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:booleans".false'                     || FALSE
        '"funcky:types".type "funcky:booleans".false' || BOOLEAN
    }

    @Unroll('Test true (expression: #expression)')
    def 'Test true'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                   || result
        '"funcky:booleans".true'                     || TRUE
        '"funcky:types".type "funcky:booleans".true' || BOOLEAN
    }

    @Unroll('Test not (expression: #expression)')
    def 'Test not'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                             || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".not)' || FALSE
        '"funcky:types".type "funcky:booleans".not'                            || FUNCTION(BOOLEAN, BOOLEAN)
        '"funcky:booleans".not "funcky:booleans".false'                        || TRUE
        '"funcky:booleans".not "funcky:booleans".true'                         || FALSE
    }

    @Unroll('Test and (expression: #expression)')
    def 'Test and'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".and)'                                  || FALSE
        '"funcky:types".type "funcky:booleans".and'                                                             || FUNCTION(BOOLEAN, BOOLEAN, BOOLEAN)
        '"funcky:types".type ("funcky:booleans".and "funcky:booleans".false)'                                   || FUNCTION(BOOLEAN, BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:booleans".and ("funcky:commons".error "foo")))' || FALSE
        '"funcky:booleans".and "funcky:booleans".false "funcky:booleans".false'                                 || FALSE
        '"funcky:booleans".and "funcky:booleans".false "funcky:booleans".true'                                  || FALSE
        '"funcky:booleans".and "funcky:booleans".true "funcky:booleans".false'                                  || FALSE
        '"funcky:booleans".and "funcky:booleans".true "funcky:booleans".true'                                   || TRUE
        '"funcky:booleans".and "funcky:booleans".false ("funcky:commons".error "foo")'                          || FALSE
    }

    @Unroll('Test or (expression: #expression)')
    def 'Test or'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                             || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".or)'                                  || FALSE
        '"funcky:types".type "funcky:booleans".or'                                                             || FUNCTION(BOOLEAN, BOOLEAN, BOOLEAN)
        '"funcky:types".type ("funcky:booleans".or "funcky:booleans".false)'                                   || FUNCTION(BOOLEAN, BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:booleans".or ("funcky:commons".error "foo")))' || FALSE
        '"funcky:booleans".or "funcky:booleans".false "funcky:booleans".false'                                 || FALSE
        '"funcky:booleans".or "funcky:booleans".false "funcky:booleans".true'                                  || TRUE
        '"funcky:booleans".or "funcky:booleans".true "funcky:booleans".false'                                  || TRUE
        '"funcky:booleans".or "funcky:booleans".true "funcky:booleans".true'                                   || TRUE
        '"funcky:booleans".or "funcky:booleans".true ("funcky:commons".error "foo")'                           || TRUE
    }

    @Unroll('Test xor (expression: #expression)')
    def 'Test xor'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".xor)'                                  || FALSE
        '"funcky:types".type "funcky:booleans".xor'                                                             || FUNCTION(BOOLEAN, BOOLEAN, BOOLEAN)
        '"funcky:types".type ("funcky:booleans".xor "funcky:booleans".false)'                                   || FUNCTION(BOOLEAN, BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:booleans".xor ("funcky:commons".error "foo")))' || FALSE
        '"funcky:booleans".xor "funcky:booleans".false "funcky:booleans".false'                                 || FALSE
        '"funcky:booleans".xor "funcky:booleans".false "funcky:booleans".true'                                  || TRUE
        '"funcky:booleans".xor "funcky:booleans".true "funcky:booleans".false'                                  || TRUE
        '"funcky:booleans".xor "funcky:booleans".true "funcky:booleans".true'                                   || FALSE
    }

    @Unroll('Test all (expression: #expression)')
    def 'Test all'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                 || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".all)'     || FALSE
        '"funcky:types".type "funcky:booleans".all'                                || FUNCTION(LIST(BOOLEAN), BOOLEAN)
        '"funcky:booleans".all []'                                                 || TRUE
        '"funcky:booleans".all ["funcky:booleans".false]'                          || FALSE
        '"funcky:booleans".all ["funcky:booleans".true]'                           || TRUE
        '"funcky:booleans".all ["funcky:booleans".false, "funcky:booleans".false]' || FALSE
        '"funcky:booleans".all ["funcky:booleans".false, "funcky:booleans".true]'  || FALSE
        '"funcky:booleans".all ["funcky:booleans".true, "funcky:booleans".false]'  || FALSE
        '"funcky:booleans".all ["funcky:booleans".true, "funcky:booleans".true]'   || TRUE
    }

    @Unroll('Test any (expression: #expression)')
    def 'Test any'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                 || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".any)'     || FALSE
        '"funcky:types".type "funcky:booleans".any'                                || FUNCTION(LIST(BOOLEAN), BOOLEAN)
        '"funcky:booleans".any []'                                                 || FALSE
        '"funcky:booleans".any ["funcky:booleans".false]'                          || FALSE
        '"funcky:booleans".any ["funcky:booleans".true]'                           || TRUE
        '"funcky:booleans".any ["funcky:booleans".false, "funcky:booleans".false]' || FALSE
        '"funcky:booleans".any ["funcky:booleans".false, "funcky:booleans".true]'  || TRUE
        '"funcky:booleans".any ["funcky:booleans".true, "funcky:booleans".false]'  || TRUE
        '"funcky:booleans".any ["funcky:booleans".true, "funcky:booleans".true]'   || TRUE
    }
}
