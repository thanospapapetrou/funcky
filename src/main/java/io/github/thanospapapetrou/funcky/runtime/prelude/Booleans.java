package io.github.thanospapapetrou.funcky.runtime.prelude;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;

public final class Booleans extends FunckyLibrary {
    public final FunckyBoolean $true = FunckyBoolean.TRUE.apply(engine);
    public final FunckyBoolean $false = FunckyBoolean.FALSE.apply(engine);

    public Booleans(final FunckyEngine engine) {
        super(engine);
    }
}
