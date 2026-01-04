package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.TRUE
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING

class InfoSpec extends BaseSpec {
    @Unroll('Test threading modes (mode: #mode)')
    def 'Test threading modes'(final String mode) {
        expect:
        engine.eval("\"funcky:info\".$mode") == toFuncky(mode.replace('_', '-'))
        engine.eval("\"funcky:types\".type \"funcky:info\".$mode") == STRING.apply(engine.context)
        where:
        mode << ['MULTITHREADED', 'THREAD_ISOLATED', 'STATELESS']
    }

    @Unroll('Test engine name (expression: #expression)')
    def 'Test engine name'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                     || result
        '"funcky:info".engineName'                     || toFuncky('funcky')
        '"funcky:types".type "funcky:info".engineName' || STRING.apply(engine.context)
    }

    @Unroll('Test engine version (expression: #expression)')
    def 'Test engine version'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                        || result
        '"funcky:info".engineVersion'                     || toFuncky('1.1.0-SNAPSHOT')
        '"funcky:types".type "funcky:info".engineVersion' || STRING.apply(engine.context)
    }

    @Unroll('Test names (expression: #expression)')
    def 'Test names'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                || result
        '"funcky:info".names'                     || toFuncky(['Funcky', 'funcky'])
        '"funcky:types".type "funcky:info".names' || LIST(STRING).apply(engine.context)
    }

    @Unroll('Test MIME types (expression: #expression)')
    def 'Test MIME types'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:info".mimeTypes'                     || toFuncky(['text/prs.funcky', 'text/x.funcky'])
        '"funcky:types".type "funcky:info".mimeTypes' || LIST(STRING).apply(engine.context)
    }

    @Unroll('Test extensions (expression: #expression)')
    def 'Test extensions'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                     || result
        '"funcky:info".extensions'                     || toFuncky(['funcky', 'fun'])
        '"funcky:types".type "funcky:info".extensions' || LIST(STRING).apply(engine.context)
    }

    @Unroll('Test threading (expression: #expression)')
    def 'Test threading'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                   || result
        '"funcky:commons".equal "funcky:info".threading "funcky:info".MULTITHREADED' || TRUE.apply(engine.context)
        '"funcky:types".type "funcky:info".threading'                                || STRING.apply(engine.context)
    }
}
