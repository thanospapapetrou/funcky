package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType
import spock.lang.Unroll

class BooleansSpec extends BaseSpec {
    @Unroll('Test false (expression: #expression)')
    def 'Test false'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:booleans".false'                     || FunckyBoolean.FALSE
        '"funcky:types".type "funcky:booleans".false' || FunckySimpleType.BOOLEAN
    }

    @Unroll('Test true (expression: #expression)')
    def 'Test true'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                   || result
        '"funcky:booleans".true'                     || FunckyBoolean.TRUE
        '"funcky:types".type "funcky:booleans".true' || FunckySimpleType.BOOLEAN
    }

    @Unroll('Test not (expression: #expression)')
    def 'Test not'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                             || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".not)' || FunckyBoolean.FALSE
        '"funcky:types".type "funcky:booleans".not'                            || new FunckyFunctionType(FunckySimpleType.BOOLEAN, FunckySimpleType.BOOLEAN)
        '"funcky:booleans".not "funcky:booleans".false'                        || FunckyBoolean.TRUE
        '"funcky:booleans".not "funcky:booleans".true'                         || FunckyBoolean.FALSE
    }

    @Unroll('Test and (expression: #expression)')
    def 'Test and'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".and)'                                  || FunckyBoolean.FALSE
        '"funcky:types".type "funcky:booleans".and'                                                             || new FunckyFunctionType(FunckySimpleType.BOOLEAN, FunckySimpleType.BOOLEAN, FunckySimpleType.BOOLEAN)
        '"funcky:types".type ("funcky:booleans".and "funcky:booleans".false)'                                   || new FunckyFunctionType(FunckySimpleType.BOOLEAN, FunckySimpleType.BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:booleans".and ("funcky:commons".error "foo")))' || FunckyBoolean.FALSE
        '"funcky:booleans".and "funcky:booleans".false "funcky:booleans".false'                                 || FunckyBoolean.FALSE
        '"funcky:booleans".and "funcky:booleans".false "funcky:booleans".true'                                  || FunckyBoolean.FALSE
        '"funcky:booleans".and "funcky:booleans".true "funcky:booleans".false'                                  || FunckyBoolean.FALSE
        '"funcky:booleans".and "funcky:booleans".true "funcky:booleans".true'                                   || FunckyBoolean.TRUE
        '"funcky:booleans".and "funcky:booleans".false ("funcky:commons".error "foo")'                          || FunckyBoolean.FALSE
    }

    @Unroll('Test or (expression: #expression)')
    def 'Test or'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                             || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".or)'                                  || FunckyBoolean.FALSE
        '"funcky:types".type "funcky:booleans".or'                                                             || new FunckyFunctionType(FunckySimpleType.BOOLEAN, FunckySimpleType.BOOLEAN, FunckySimpleType.BOOLEAN)
        '"funcky:types".type ("funcky:booleans".or "funcky:booleans".false)'                                   || new FunckyFunctionType(FunckySimpleType.BOOLEAN, FunckySimpleType.BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:booleans".or ("funcky:commons".error "foo")))' || FunckyBoolean.FALSE
        '"funcky:booleans".or "funcky:booleans".false "funcky:booleans".false'                                 || FunckyBoolean.FALSE
        '"funcky:booleans".or "funcky:booleans".false "funcky:booleans".true'                                  || FunckyBoolean.TRUE
        '"funcky:booleans".or "funcky:booleans".true "funcky:booleans".false'                                  || FunckyBoolean.TRUE
        '"funcky:booleans".or "funcky:booleans".true "funcky:booleans".true'                                   || FunckyBoolean.TRUE
        '"funcky:booleans".or "funcky:booleans".true ("funcky:commons".error "foo")'                           || FunckyBoolean.TRUE
    }

    @Unroll('Test xor (expression: #expression)')
    def 'Test xor'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".xor)'                                  || FunckyBoolean.FALSE
        '"funcky:types".type "funcky:booleans".xor'                                                             || new FunckyFunctionType(FunckySimpleType.BOOLEAN, FunckySimpleType.BOOLEAN, FunckySimpleType.BOOLEAN)
        '"funcky:types".type ("funcky:booleans".xor "funcky:booleans".false)'                                   || new FunckyFunctionType(FunckySimpleType.BOOLEAN, FunckySimpleType.BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:booleans".xor ("funcky:commons".error "foo")))' || FunckyBoolean.FALSE
        '"funcky:booleans".xor "funcky:booleans".false "funcky:booleans".false'                                 || FunckyBoolean.FALSE
        '"funcky:booleans".xor "funcky:booleans".false "funcky:booleans".true'                                  || FunckyBoolean.TRUE
        '"funcky:booleans".xor "funcky:booleans".true "funcky:booleans".false'                                  || FunckyBoolean.TRUE
        '"funcky:booleans".xor "funcky:booleans".true "funcky:booleans".true'                                   || FunckyBoolean.FALSE
    }

    @Unroll('Test all (expression: #expression)')
    def 'Test all'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                 || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".all)'     || FunckyBoolean.FALSE
        '"funcky:types".type "funcky:booleans".all'                                || new FunckyFunctionType(new FunckyListType(FunckySimpleType.BOOLEAN), FunckySimpleType.BOOLEAN)
        '"funcky:booleans".all []'                                                 || FunckyBoolean.TRUE
        '"funcky:booleans".all ["funcky:booleans".false]'                          || FunckyBoolean.FALSE
        '"funcky:booleans".all ["funcky:booleans".true]'                           || FunckyBoolean.TRUE
        '"funcky:booleans".all ["funcky:booleans".false, "funcky:booleans".false]' || FunckyBoolean.FALSE
        '"funcky:booleans".all ["funcky:booleans".false, "funcky:booleans".true]'  || FunckyBoolean.FALSE
        '"funcky:booleans".all ["funcky:booleans".true, "funcky:booleans".false]'  || FunckyBoolean.FALSE
        '"funcky:booleans".all ["funcky:booleans".true, "funcky:booleans".true]'   || FunckyBoolean.TRUE
    }

    @Unroll('Test any (expression: #expression)')
    def 'Test any'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                 || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:booleans".any)'     || FunckyBoolean.FALSE
        '"funcky:types".type "funcky:booleans".any'                                || new FunckyFunctionType(new FunckyListType(FunckySimpleType.BOOLEAN), FunckySimpleType.BOOLEAN)
        '"funcky:booleans".any []'                                                 || FunckyBoolean.FALSE
        '"funcky:booleans".any ["funcky:booleans".false]'                          || FunckyBoolean.FALSE
        '"funcky:booleans".any ["funcky:booleans".true]'                           || FunckyBoolean.TRUE
        '"funcky:booleans".any ["funcky:booleans".false, "funcky:booleans".false]' || FunckyBoolean.FALSE
        '"funcky:booleans".any ["funcky:booleans".false, "funcky:booleans".true]'  || FunckyBoolean.TRUE
        '"funcky:booleans".any ["funcky:booleans".true, "funcky:booleans".false]'  || FunckyBoolean.TRUE
        '"funcky:booleans".any ["funcky:booleans".true, "funcky:booleans".true]'   || FunckyBoolean.TRUE
    }
}
