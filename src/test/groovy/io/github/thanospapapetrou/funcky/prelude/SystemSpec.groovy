package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral
import io.github.thanospapapetrou.funcky.runtime.FunckyMonad
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType
import io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType

import java.nio.charset.Charset

class SystemSpec extends BaseSpec {
    def 'Test JVM name'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".jvmName')).getBase().eval(engine.context).toString() == System.getProperty('java.vm.name')
        engine.eval('"funcky:types".type "funcky:system".jvmName') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test JVM version'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".jvmVersion')).getBase().eval(engine.context).toString() == System.getProperty('java.vm.version')
        engine.eval('"funcky:types".type "funcky:system".jvmVersion') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test JVM vendor'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".jvmVendor')).getBase().eval(engine.context).toString() == System.getProperty('java.vm.vendor')
        engine.eval('"funcky:types".type "funcky:system".jvmVendor') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test OS name'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".osName')).getBase().eval(engine.context).toString() == System.getProperty('os.name')
        engine.eval('"funcky:types".type "funcky:system".osName') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test OS version'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".osVersion')).getBase().eval(engine.context).toString() == System.getProperty('os.version')
        engine.eval('"funcky:types".type "funcky:system".osVersion') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test OS architecture'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".osArchitecture')).getBase().eval(engine.context).toString() == System.getProperty('os.arch')
        engine.eval('"funcky:types".type "funcky:system".osArchitecture') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test line separator'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".lineSeparator')).getBase().eval(engine.context).toString() == System.lineSeparator()
        engine.eval('"funcky:types".type "funcky:system".lineSeparator') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test file separator'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".fileSeparator')).getBase().eval(engine.context).toString() == File.separator
        engine.eval('"funcky:types".type "funcky:system".fileSeparator') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test path separator'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".pathSeparator')).getBase().eval(engine.context).toString() == File.pathSeparator
        engine.eval('"funcky:types".type "funcky:system".pathSeparator') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test encoding'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".encoding')).getBase().eval(engine.context).toString() == Charset.defaultCharset().name()
        engine.eval('"funcky:types".type "funcky:system".encoding') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test locale'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".locale')).getBase().eval(engine.context).toString() == Locale.getDefault().toString()
        engine.eval('"funcky:types".type "funcky:system".locale') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test time zone'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".timeZone')).getBase().eval(engine.context).toString() == TimeZone.getDefault().getID()
        engine.eval('"funcky:types".type "funcky:system".timeZone') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }

    def 'Test username'() {
        expect:
        ((FunckyMonad) engine.eval('"funcky:system".username')).getBase().eval(engine.context).toString() == System.getProperty('user.name')
        engine.eval('"funcky:types".type "funcky:system".username') == FunckyMonadicType.io(engine.context, new FunckyLiteral(engine, FunckyListType.STRING.apply(engine.context)))
    }
}
