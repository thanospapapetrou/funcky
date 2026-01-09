package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.io.File;
import java.nio.charset.Charset;
import java.util.Locale;
import java.util.TimeZone;
import java.util.function.Supplier;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyMonad;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType;

public final class System extends FunckyLibrary {
    private static final String PROPERTY_JVM_NAME = "java.vm.name";
    private static final String PROPERTY_JVM_VERSION = "java.vm.version";
    private static final String PROPERTY_JVM_VENDOR = "java.vm.vendor";
    private static final String PROPERTY_OS_NAME = "os.name";
    private static final String PROPERTY_OS_VERSION = "os.version";
    private static final String PROPERTY_OS_ARCHITECTURE = "os.arch";
    private static final String PROPERTY_USERNAME = "user.name";

    public final FunckyValue jvmName = get(PROPERTY_JVM_NAME);
    public final FunckyValue jvmVersion = get(PROPERTY_JVM_VERSION);
    public final FunckyValue jvmVendor = get(PROPERTY_JVM_VENDOR);
    public final FunckyValue osName = get(PROPERTY_OS_NAME);
    public final FunckyValue osVersion = get(PROPERTY_OS_VERSION);
    public final FunckyValue osArchitecture = get(PROPERTY_OS_ARCHITECTURE);
    public final FunckyValue lineSeparator = get(java.lang.System::lineSeparator);
    public final FunckyValue fileSeparator = get(() -> File.separator);
    public final FunckyValue pathSeparator = get(() -> File.pathSeparator);
    public final FunckyValue encoding = get(() -> Charset.defaultCharset().name());
    public final FunckyValue locale = get(() -> Locale.getDefault().toString());
    public final FunckyValue timeZone = get(() -> TimeZone.getDefault().getID());
    public final FunckyValue username = get(PROPERTY_USERNAME);

    public System(final FunckyContext context) {
        super(context);
    }

    private FunckyValue get(final Supplier<String> value) {
        return new FunckyMonad(context, FunckyMonadicType.io(context,
                new FunckyLiteral(FunckyListType.STRING.apply(context))),
                () -> new FunckyLiteral(context.getEngine().toFuncky(value.get())));
    }

    private FunckyValue get(final String property) {
        return get(() -> java.lang.System.getProperty(property));
    }
}
