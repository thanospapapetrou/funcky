package io.github.thanospapapetrou.funcky.runtime

import io.github.thanospapapetrou.funcky.FunckyFactory
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext
import io.github.thanospapapetrou.funcky.runtime.prelude.Numbers
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType
import spock.lang.Specification

class FunckyCharacterSpec extends Specification {
    private static final FunckyBoolean BOOLEAN = FunckyBoolean.FALSE.apply(new FunckyFactory().scriptEngine.context)
    private static final FunckyCharacter CHARACTER = new FunckyCharacter(new FunckyFactory().scriptEngine.context, 'A' as char)
    private static final FunckyFunction FUNCTION = new Numbers(new FunckyFactory().scriptEngine.context).add
    private static final FunckyCharacter OTHER_CHARACTER = new FunckyCharacter(new FunckyFactory().scriptEngine.context, 'b' as char)
    private static final char VALUE = 'a'

    private FunckyCharacter character

    def setup() {
        character = new FunckyCharacter(Mock(FunckyContext), VALUE)
    }

    def 'Test constructor'() {
        given:
        final FunckyContext context = Mock()
        when:
        final FunckyCharacter result = new FunckyCharacter(context, VALUE)
        then:
        result
        result.context == context
        result.value == VALUE
    }

    def 'Test getType()'() {
        expect:
        character.type == FunckySimpleType.CHARACTER.apply(character.context)
    }

    def 'Test toExpression()'() {
        when:
        final FunckyExpression result = character.toExpression()
        then:
        result
        (result as FunckyLiteral).value == character
    }

    def 'Test compareTo()'(final FunckyValue value, final int comparison) {
        expect:
        character.compareTo(value) == comparison
        where:
        value                                                                || comparison
        new FunckyCharacter(new FunckyFactory().scriptEngine.context, VALUE) || 0
        CHARACTER                                                            || Character.compare(VALUE, CHARACTER.value)
        OTHER_CHARACTER                                                      || Character.compare(VALUE, OTHER_CHARACTER.value)
        BOOLEAN                                                              || 1
        FUNCTION                                                             || -1
    }

    def 'Test equals()'(final Object value, final boolean equality) {
        expect:
        character.equals(value) == equality
        where:
        value                                                                || equality
        new FunckyCharacter(new FunckyFactory().scriptEngine.context, VALUE) || true
        new Object()                                                         || false
        CHARACTER                                                            || false
        OTHER_CHARACTER                                                      || false
        BOOLEAN                                                              || false
        FUNCTION                                                             || false
    }

    def 'Test hashCode()'() {
        expect:
        character.hashCode() == Character.hashCode(VALUE)
    }

    def 'Test toString()'() {
        expect:
        character.toString() == Character.toString(VALUE)
    }
}
