package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyMonad

import java.nio.charset.Charset

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType.IO;

class SystemSpec extends BaseSpec {
    def 'Test JVM name'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".jvmName')).getBase().eval(engine.context).toString() == System.getProperty('java.vm.name')
        engine.eval('"funcky:types".type "funcky:system".jvmName') == IO(STRING).apply(engine.context)
    }

    def 'Test JVM version'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".jvmVersion')).getBase().eval(engine.context).toString() == System.getProperty('java.vm.version')
        engine.eval('"funcky:types".type "funcky:system".jvmVersion') == IO(STRING).apply(engine.context)
    }

    def 'Test JVM vendor'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".jvmVendor')).getBase().eval(engine.context).toString() == System.getProperty('java.vm.vendor')
        engine.eval('"funcky:types".type "funcky:system".jvmVendor') == IO(STRING).apply(engine.context)
    }

    def 'Test OS name'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".osName')).getBase().eval(engine.context).toString() == System.getProperty('os.name')
        engine.eval('"funcky:types".type "funcky:system".osName') == IO(STRING).apply(engine.context)
    }

    def 'Test OS version'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".osVersion')).getBase().eval(engine.context).toString() == System.getProperty('os.version')
        engine.eval('"funcky:types".type "funcky:system".osVersion') == IO(STRING).apply(engine.context)
    }

    def 'Test OS architecture'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".osArchitecture')).getBase().eval(engine.context).toString() == System.getProperty('os.arch')
        engine.eval('"funcky:types".type "funcky:system".osArchitecture') == IO(STRING).apply(engine.context)
    }

    def 'Test line separator'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".lineSeparator')).getBase().eval(engine.context).toString() == System.lineSeparator()
        engine.eval('"funcky:types".type "funcky:system".lineSeparator') == IO(STRING).apply(engine.context)
    }

    def 'Test file separator'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".fileSeparator')).getBase().eval(engine.context).toString() == File.separator
        engine.eval('"funcky:types".type "funcky:system".fileSeparator') == IO(STRING).apply(engine.context)
    }

    def 'Test path separator'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".pathSeparator')).getBase().eval(engine.context).toString() == File.pathSeparator
        engine.eval('"funcky:types".type "funcky:system".pathSeparator') == IO(STRING).apply(engine.context)
    }

    def 'Test encoding'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".encoding')).getBase().eval(engine.context).toString() == Charset.defaultCharset().name()
        engine.eval('"funcky:types".type "funcky:system".encoding') == IO(STRING).apply(engine.context)
    }

    def 'Test locale'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".locale')).getBase().eval(engine.context).toString() == Locale.getDefault().toString()
        engine.eval('"funcky:types".type "funcky:system".locale') == IO(STRING).apply(engine.context)
    }

    def 'Test time zone'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".timeZone')).getBase().eval(engine.context).toString() == TimeZone.getDefault().getID()
        engine.eval('"funcky:types".type "funcky:system".timeZone') == IO(STRING).apply(engine.context)
    }

    def 'Test username'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".username')).getBase().eval(engine.context).toString() == System.getProperty('user.name')
        engine.eval('"funcky:types".type "funcky:system".username') == IO(STRING).apply(engine.context)
    }
}
