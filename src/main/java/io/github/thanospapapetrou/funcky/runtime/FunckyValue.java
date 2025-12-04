package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public sealed abstract class FunckyValue
        permits FunckyType, FunckyNumber, FunckyBoolean, FunckyCharacter, FunckyFunction, FunckyList, FunckyRecord {
    protected final FunckyEngine engine;

    protected FunckyValue(final FunckyEngine engine) {
        this.engine = engine;
    }

    public FunckyEngine getEngine() {
        return engine;
    }

    public abstract FunckyType getType();

    public abstract FunckyExpression toExpression();

    @Override
    public String toString() {
        return toExpression().normalize().toString();
    }
}
