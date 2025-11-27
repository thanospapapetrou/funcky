package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;

public sealed abstract class FunckyValue
        permits FunckyType, FunckyNumber, FunckyBoolean, FunckyCharacter, FunckyFunction, FunckyList, FunckyRecord {
    public abstract FunckyType getType();

    public FunckyExpression toExpression() {
        return new FunckyLiteral(this);
    }

    @Override
    public String toString() {
        return toExpression().normalize().toString();
    }
}
