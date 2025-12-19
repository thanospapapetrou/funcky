package io.github.thanospapapetrou.funcky.runtime.prelude;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;

public final class Booleans extends FunckyLibrary {
    public final FunckyBoolean _false = FunckyBoolean.FALSE.apply(engine);
    public final FunckyBoolean _true = FunckyBoolean.TRUE.apply(engine);

    public Booleans(final FunckyEngine engine) {
        super(engine);
    }
}
