package io.github.thanospapapetrou.funcky.runtime.prelude;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;

public final class Booleans extends FunckyLibrary {
    public Booleans(final FunckyEngine engine) {
        super(engine, FunckyBoolean.FALSE.apply(engine), FunckyBoolean.TRUE.apply(engine));
    }
}
