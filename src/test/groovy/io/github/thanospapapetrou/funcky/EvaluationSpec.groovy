package io.github.thanospapapetrou.funcky

import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import spock.lang.Unroll

class EvaluationSpec extends BaseSpec {
    def 'Test evaluate empty expression'() {
        expect:
        engine.eval('') == null
    }

    @Unroll('Test evaluate number literal (expression: #expression.key)')
    def 'Test evaluate number literal'(final Map.Entry<String, FunckyValue> expression) {
        expect:
        engine.eval(expression.key) == expression.value
        where:
        expression << NUMBERS
    }

    @Unroll('Test evaluate character literal (expression: #expression.key)')
    def 'Test evaluate character literal'(final Map.Entry<String, FunckyValue> expression) {
        expect:
        engine.eval(expression.key) == expression.value
        where:
        expression << CHARACTERS
    }

    @Unroll('Test evaluate string literal (expression: #expression.key)')
    def 'Test evaluate string literal'(final Map.Entry<String, FunckyValue> expression) {
        expect:
        engine.eval(expression.key) == expression.value
        where:
        expression << strings
    }
}
