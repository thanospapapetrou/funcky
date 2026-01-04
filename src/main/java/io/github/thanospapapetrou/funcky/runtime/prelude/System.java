package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.io.File;
import java.nio.charset.Charset;
import java.util.Locale;
import java.util.TimeZone;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public final class System extends FunckyLibrary {
    public final FunckyValue jvmName = get(FunckyEngine.PARAMETER_JVM_NAME);
    public final FunckyValue jvmVersion = get(FunckyEngine.PARAMETER_JVM_VERSION);
    public final FunckyValue jvmVendor = get(FunckyEngine.PARAMETER_JVM_VENDOR);
    public final FunckyValue osName = get(FunckyEngine.PARAMETER_OS_NAME);
    public final FunckyValue osVersion = get(FunckyEngine.PARAMETER_OS_VERSION);
    public final FunckyValue osArchitecture = get(FunckyEngine.PARAMETER_OS_ARCHITECTURE);
    public final FunckyValue lineSeparator = context.getEngine().toFuncky(java.lang.System.lineSeparator());
    public final FunckyValue fileSeparator = context.getEngine().toFuncky(File.separator);
    public final FunckyValue pathSeparator = context.getEngine().toFuncky(File.pathSeparator);
    public final FunckyValue encoding = context.getEngine().toFuncky(Charset.defaultCharset().name());
    public final FunckyValue locale = context.getEngine().toFuncky(Locale.getDefault().toString());
    public final FunckyValue timeZone = context.getEngine().toFuncky(TimeZone.getDefault().getID());
    public final FunckyValue username = get(FunckyEngine.PARAMETER_USERNAME);

    public System(final FunckyContext context) {
        super(context);
    }

    private FunckyValue get(final String key) {
        return (FunckyValue) context.getBindings(ScriptContext.ENGINE_SCOPE).get(key);
    }
}
