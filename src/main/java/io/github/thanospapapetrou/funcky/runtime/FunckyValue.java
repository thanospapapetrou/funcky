package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;

public sealed abstract class FunckyValue
        permits FunckyType, FunckyNumber, FunckyBoolean, FunckyCharacter, FunckyFunction, FunckyList, FunckyRecord {
    public abstract FunckyType getType();

    public abstract FunckyExpression toExpression();

    @Override
    public String toString() {
        return toExpression().normalize().toString();
    }
}
