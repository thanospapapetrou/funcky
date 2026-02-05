package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER

class IOSpec extends BaseSpec {
    @Unroll('Test standard streams (stream: #stream)')
    def 'Test standard streams'(final Map.Entry<String, BigDecimal> stream) {
        expect:
        engine.eval("\"funcky:io\".$stream.value") == toFuncky(new BigDecimal(stream.key))
        engine.eval("\"funcky:types\".type \"funcky:io\".$stream.value") == NUMBER.apply(engine.context)
        where:
        stream << ['STDIN', 'STDOUT', 'STDERR'].indexed()
    }
}
