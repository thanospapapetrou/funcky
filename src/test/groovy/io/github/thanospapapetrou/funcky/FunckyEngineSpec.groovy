package io.github.thanospapapetrou.funcky

import javax.script.Bindings
import spock.lang.Specification

class FunckyEngineSpec extends Specification {
    private FunckyFactory factory
    private FunckyEngine engine

    def setup() {
        factory = new FunckyFactory()
        engine = factory.scriptEngine
    }

    def 'Test create bindings'() {
        when:
        final Bindings bindings = engine.createBindings()
        then:
        bindings != null
        bindings.isEmpty()
    }

    def 'Test get factory'() {
        expect:
        engine.factory == factory
    }
}
