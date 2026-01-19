package io.github.thanospapapetrou.funcky.runtime

import io.github.thanospapapetrou.funcky.FunckyFactory
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper
import io.github.thanospapapetrou.funcky.runtime.prelude.Booleans
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType
import spock.lang.Specification
import spock.lang.Unroll

class FunckyBooleanSpec extends Specification {
    private static final FunckyCharacter CHARACTER = new FunckyCharacter(new FunckyFactory().scriptEngine.context, 'a' as char)
    private static final FunckyBoolean FALSE = FunckyBoolean.FALSE.apply(new FunckyFactory().scriptEngine.context)
    private static final FunckyNumber NUMBER = new FunckyNumber(new FunckyFactory().scriptEngine.context, 0.0G)
    private static final FunckyBoolean TRUE = FunckyBoolean.TRUE.apply(new FunckyFactory().scriptEngine.context)

    private FunckyContext context

    def setup() {
        context = Mock(FunckyContext)
    }

    @Unroll('Test constructor (boolean: #bool)')
    def 'Test constructor'(final boolean bool) {
        given:
        final FunckyContext context = Mock()
        when:
        final FunckyBoolean result = new FunckyBoolean(context, bool)
        then:
        result
        result.context == context
        result.value == bool
        where:
        bool << [false, true]
    }

    @Unroll('Test getType() (boolean: #bool)')
    def 'Test getType()'(final boolean bool) {
        expect:
        new FunckyBoolean(context, bool).type == FunckySimpleType.BOOLEAN.apply(context)
        where:
        bool << [false, true]
    }

    @Unroll('Test toExpression() (boolean: #bool)')
    def 'Test toExpression()'(final boolean bool) {
        when:
        final FunckyExpression result = new FunckyBoolean(context, bool).toExpression()
        then:
        result
        final FunckyReference reference = (result as FunckyReference)
        reference.namespace == FunckyLibrary.getNamespace(Booleans.class)
        reference.prefix == null
        reference.canonical == FunckyLibrary.getNamespace(Booleans.class)
        reference.name == Boolean.toString(bool)
        where:
        bool << [false, true]
    }

    @Unroll('Test compareTo() (boolean: #bool, value: #value)')
    def 'Test compareTo()'(final boolean bool, final FunckyValue value, final int comparison) {
        expect:
        new FunckyBoolean(context, bool).compareTo(value) == comparison
        where:
        bool  | value     || comparison
        false | NUMBER    || 1
        false | FALSE     || 0
        false | TRUE      || -1
        false | CHARACTER || -1
        true  | NUMBER    || 1
        true  | FALSE     || 1
        true  | TRUE      || 0
        true  | CHARACTER || -1
    }

    @Unroll('Test equals() (boolean: #bool, value: #value)')
    def 'Test equals()'(final boolean bool, final Object value, final boolean equality) {
        expect:
        new FunckyBoolean(context, bool).equals(value) == equality
        where:
        bool  | value        || equality
        false | new Object() || false
        false | NUMBER       || false
        false | FALSE        || true
        false | TRUE         || false
        false | CHARACTER    || false
        true  | new Object() || false
        true  | NUMBER       || false
        true  | FALSE        || false
        true  | TRUE         || true
        true  | CHARACTER    || false
    }

    @Unroll('Test hashCode() (boolean: #bool)')
    def 'Test hashCode()'(final boolean bool) {
        expect:
        new FunckyBoolean(context, bool).hashCode() == Boolean.hashCode(bool)
        where:
        bool << [false, true]
    }

    @Unroll('Test toString() (boolean: #bool)')
    def 'Test toString()'(final boolean bool) {
        expect:
        new FunckyBoolean(context, bool).toString() == "\"${EscapeHelper.escape(FunckyLibrary.getNamespace(Booleans).toString())}\".${Boolean.toString(bool)}"
        where:
        bool << [false, true]
    }
}
