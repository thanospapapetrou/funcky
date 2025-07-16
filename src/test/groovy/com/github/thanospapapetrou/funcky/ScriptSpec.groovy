package com.github.thanospapapetrou.funcky

import com.github.thanospapapetrou.funcky.runtime.FunckyNumber
import spock.lang.Unroll

class ScriptSpec extends BaseSpec {
    @Unroll('Test evaluate script (script: #script, arguments: #arguments)')
    def 'Test evaluate script'(final String script, final String[] arguments, final BigDecimal result) {
        given:
        final Reader reader = setScript(script, arguments)
        expect:
        engine.eval(reader) == new FunckyNumber(result)
        cleanup:
        reader.close()
        where:
        script                      | arguments || result
        '/count_arguments.funcky'   | []        || 0.0G
        '/count_arguments.funcky'   | ['foo']   || 1.0G
        '/count_arguments_1.funcky' | []        || 0.0G
        '/count_arguments_1.funcky' | ['foo']   || 1.0G
    }

    @Unroll('Test evaluate script as expression (expression: #expression)')
    def 'Test evaluate script as expression'(final String expression, final BigDecimal result) {
        expect:
        engine.eval(expression) == new FunckyNumber(result)
        where:
        expression                                                      || result
        '"./target/test-classes/count_arguments.funcky".main []'        || 0.0G
        '"./target/test-classes/count_arguments.funcky".main ["foo"]'   || 1.0G
        '"./target/test-classes/count_arguments_1.funcky".main []'      || 0.0G
        '"./target/test-classes/count_arguments_1.funcky".main ["foo"]' || 1.0G
    }
}
