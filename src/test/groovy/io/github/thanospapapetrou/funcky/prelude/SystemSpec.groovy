package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import spock.lang.Unroll

import java.nio.charset.Charset

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING

class SystemSpec extends BaseSpec {
    @Unroll('Test JVM name (expression: #expression)')
    def 'Test JVM name'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:system".jvmName'                     || toFuncky(System.getProperty('java.vm.name'))
        '"funcky:types".type "funcky:system".jvmName' || STRING.apply(engine.context)
    }

    @Unroll('Test JVM version (expression: #expression)')
    def 'Test JVM version'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                       || result
        '"funcky:system".jvmVersion'                     || toFuncky(System.getProperty('java.vm.version'))
        '"funcky:types".type "funcky:system".jvmVersion' || STRING.apply(engine.context)
    }

    @Unroll('Test JVM vendor (expression: #expression)')
    def 'Test JVM vendor'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                      || result
        '"funcky:system".jvmVendor'                     || toFuncky(System.getProperty('java.vm.vendor'))
        '"funcky:types".type "funcky:system".jvmVendor' || STRING.apply(engine.context)
    }

    @Unroll('Test OS name (expression: #expression)')
    def 'Test OS name'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                   || result
        '"funcky:system".osName'                     || toFuncky(System.getProperty('os.name'))
        '"funcky:types".type "funcky:system".osName' || STRING.apply(engine.context)
    }

    @Unroll('Test OS version (expression: #expression)')
    def 'Test OS version'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                      || result
        '"funcky:system".osVersion'                     || toFuncky(System.getProperty('os.version'))
        '"funcky:types".type "funcky:system".osVersion' || STRING.apply(engine.context)
    }

    @Unroll('Test OS architecture (expression: #expression)')
    def 'Test OS architecture'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                           || result
        '"funcky:system".osArchitecture'                     || toFuncky(System.getProperty('os.arch'))
        '"funcky:types".type "funcky:system".osArchitecture' || STRING.apply(engine.context)
    }

    @Unroll('Test line separator (expression: #expression)')
    def 'Test line separator'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                          || result
        '"funcky:system".lineSeparator'                     || toFuncky(System.lineSeparator())
        '"funcky:types".type "funcky:system".lineSeparator' || STRING.apply(engine.context)
    }

    @Unroll('Test file separator (expression: #expression)')
    def 'Test file separator'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                          || result
        '"funcky:system".fileSeparator'                     || toFuncky(File.separator)
        '"funcky:types".type "funcky:system".fileSeparator' || STRING.apply(engine.context)
    }

    @Unroll('Test path separator (expression: #expression)')
    def 'Test path separator'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                          || result
        '"funcky:system".pathSeparator'                     || toFuncky(File.pathSeparator)
        '"funcky:types".type "funcky:system".pathSeparator' || STRING.apply(engine.context)
    }

    @Unroll('Test encoding (expression: #expression)')
    def 'Test encoding'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                     || result
        '"funcky:system".encoding'                     || toFuncky(Charset.defaultCharset().name())
        '"funcky:types".type "funcky:system".encoding' || STRING.apply(engine.context)
    }

    @Unroll('Test locale (expression: #expression)')
    def 'Test locale'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                   || result
        '"funcky:system".locale'                     || toFuncky(Locale.getDefault().toString())
        '"funcky:types".type "funcky:system".locale' || STRING.apply(engine.context)
    }

    @Unroll('Test time zone (expression: #expression)')
    def 'Test time zone'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                     || result
        '"funcky:system".timeZone'                     || toFuncky(TimeZone.getDefault().getID())
        '"funcky:types".type "funcky:system".timeZone' || STRING.apply(engine.context)
    }

    @Unroll('Test user name (expression: #expression)')
    def 'Test user name'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                     || result
        '"funcky:system".username'                     || toFuncky(System.getProperty('user.name'))
        '"funcky:types".type "funcky:system".username' || STRING.apply(engine.context)
    }
}
