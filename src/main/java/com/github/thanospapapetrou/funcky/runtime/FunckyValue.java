package com.github.thanospapapetrou.funcky.runtime;

import com.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import com.github.thanospapapetrou.funcky.compiler.linker.exceptions.UnboundPrefixException;
import com.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import com.github.thanospapapetrou.funcky.runtime.exceptions.SneakyFunckyRuntimeException;
import com.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public abstract class FunckyValue {
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
