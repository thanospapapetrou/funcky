package io.github.thanospapapetrou.funcky.runtime

import io.github.thanospapapetrou.funcky.FunckyFactory
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType
import spock.lang.Specification
import spock.lang.Unroll

class FunckyNumberSpec extends Specification {
    private static final FunckyBoolean BOOLEAN = FunckyBoolean.FALSE.apply(new FunckyFactory().scriptEngine.context)
    private static final FunckyNumber NUMBER = new FunckyNumber(new FunckyFactory().scriptEngine.context, -1.0G)
    private static final FunckyNumber OTHER_NUMBER = new FunckyNumber(new FunckyFactory().scriptEngine.context, 1.0G)
    private static final FunckyType TYPE = FunckySimpleType.TYPE.apply(new FunckyFactory().scriptEngine.context)
    private static final BigDecimal VALUE = 0.0G

    private FunckyNumber number

    def setup() {
        number = new FunckyNumber(Mock(FunckyContext), VALUE)
    }

    def 'Test constructor'() {
        given:
        final FunckyContext context = Mock()
        when:
        final FunckyNumber result = new FunckyNumber(context, VALUE)
        then:
        result
        result.context == context
        result.value == VALUE
    }

    def 'Test getType()'() {
        expect:
        number.type == FunckySimpleType.NUMBER.apply(number.context)
    }

    def 'Test toExpression()'() {
        when:
        final FunckyExpression result = number.toExpression()
        then:
        result
        (result as FunckyLiteral).value == number
    }

    @Unroll('Test compareTo() (value: #value)')
    def 'Test compareTo()'(final FunckyValue value, final int comparison) {
        expect:
        number.compareTo(value) == comparison
        where:
        value                                                             || comparison
        new FunckyNumber(new FunckyFactory().scriptEngine.context, VALUE) || 0
        NUMBER                                                            || VALUE <=> NUMBER.value
        OTHER_NUMBER                                                      || VALUE <=> OTHER_NUMBER.value
        TYPE                                                              || 1
        BOOLEAN                                                           || -1
    }

    @Unroll('Test equals() (value: #value)')
    def 'Test equals()'(final Object value, final boolean equality) {
        expect:
        number.equals(value) == equality
        where:
        value                                                             || equality
        new FunckyNumber(new FunckyFactory().scriptEngine.context, VALUE) || true
        new Object()                                                      || false
        NUMBER                                                            || false
        OTHER_NUMBER                                                      || false
        TYPE                                                              || false
        BOOLEAN                                                           || false
    }

    def 'Test hashCode()'() {
        expect:
        number.hashCode() == Double.hashCode(VALUE.doubleValue())
    }

    def 'Test toString()'() {
        expect:
        number.toString() == VALUE.toString()
    }
}
