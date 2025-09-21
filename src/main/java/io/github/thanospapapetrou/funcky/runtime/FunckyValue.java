package io.github.thanospapapetrou.funcky.runtime;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnboundPrefixException;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyFunckyRuntimeException;

public sealed abstract class FunckyValue
        permits FunckyType, FunckyNumber, FunckyBoolean, FunckyCharacter, FunckyFunction, FunckyList, FunckyRecord {
    public abstract FunckyType getType();

    public abstract FunckyExpression toExpression();

    @Override
    public String toString() {
        try {
            return toExpression().normalize().toString();
        } catch (final UnboundPrefixException e) {
            throw new SneakyFunckyRuntimeException(new FunckyRuntimeException(e));
        }
    }
}
