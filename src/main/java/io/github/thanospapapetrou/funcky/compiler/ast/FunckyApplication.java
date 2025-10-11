package io.github.thanospapapetrou.funcky.compiler.ast;

import java.util.Map;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.compiler.exceptions.IllegalApplicationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public final class FunckyApplication extends FunckyExpression {
    private static final String FORMAT_APPLICATION = "%1$s %2$s";
    private static final String FORMAT_NESTED_APPLICATION = "%1$s (%2$s)";
    private static final String JAVA = "new %1$s(%2$s, %3$s)";

    private final FunckyExpression function;
    private final FunckyExpression argument;

    public FunckyApplication(final FunckyExpression function, final FunckyExpression argument) {
        super(function.engine, function.file, function.line, function.column);
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
    public FunckyApplication normalize() {
        return new FunckyApplication(function.normalize(), argument.normalize());
    }

    @Override
    public String toJava() {
        return String.format(JAVA, FunckyApplication.class.getName(), function.toJava(), argument.toJava());
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
        return String.format(((argument instanceof FunckyApplication)
                || ((argument instanceof FunckyLiteral)
                && (((FunckyLiteral) argument).getValue().toExpression() instanceof FunckyApplication)))
                ? FORMAT_NESTED_APPLICATION
                : FORMAT_APPLICATION, function, argument);
    }

    @Override
    protected FunckyType getType(final Map<FunckyReference, FunckyTypeVariable> assumptions) {
        final FunckyType functionType = function.getType(assumptions);
        final FunckyType argumentType = argument.getType(assumptions);
            final FunckyFunctionType type = (FunckyFunctionType) functionType.unify(
                    new FunckyFunctionType(engine, new FunckyLiteral(engine, argumentType), new FunckyLiteral(engine, new FunckyTypeVariable(engine))));
            if (type != null) {
                return ((FunckyType) type.getRange().eval(engine.getContext()));
            } else {
                throw new SneakyCompilationException(new IllegalApplicationException(this, functionType, argumentType));
            }
    }
}
