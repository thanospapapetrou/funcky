package io.github.thanospapapetrou.funcky.compiler.linker;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public final class FunckyApplication extends FunckyExpression {
    private static final String FORMAT_APPLICATION = "%1$s %2$s";
    private static final String FORMAT_NESTED_APPLICATION = "%1$s (%2$s)";

    private final FunckyExpression function;
    private final FunckyExpression argument;

    public FunckyApplication(final FunckyExpression function, final FunckyExpression argument) {
        this(function, argument, null); // TODO provide types where required
    }

    public FunckyApplication(final FunckyExpression function, final FunckyExpression argument, final FunckyType type) {
        super(function.engine, function.file, function.line, function.column, type);
        this.function = function;
        this.argument = argument;
    }

    public FunckyExpression getFunction() {
        return function;
    }

    public FunckyExpression getArgument() {
        return argument;
    }

    @Override
    public FunckyValue eval(final ScriptContext context) {
        try {
            return ((FunckyFunction) function.eval(context)).apply(argument, context);
        } catch (final SneakyRuntimeException e) {
            e.getCause().getStack().add(this);
            throw e;
        }
    }

    @Override
    public String toString() {
        return String.format(((argument instanceof FunckyApplication) || ((argument instanceof FunckyLiteral)
                && (argument.eval().toExpression() instanceof FunckyApplication)))
                ? FORMAT_NESTED_APPLICATION : FORMAT_APPLICATION, function, argument);
    }
}
