package io.github.thanospapapetrou.funcky.runtime.prelude;

import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;

public final class Booleans extends FunckyLibrary {
    public final FunckyBoolean _false = FunckyBoolean.FALSE.apply(context);
    public final FunckyBoolean _true = FunckyBoolean.TRUE.apply(context);

    public Booleans(final FunckyContext context) {
        super(context);
    }
}
