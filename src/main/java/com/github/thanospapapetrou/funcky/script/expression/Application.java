package com.github.thanospapapetrou.funcky.script.expression;

import com.github.thanospapapetrou.funcky.FunckyEngine;
import com.github.thanospapapetrou.funcky.script.expression.literal.Function;
import com.github.thanospapapetrou.funcky.script.expression.literal.Literal;
import com.github.thanospapapetrou.funcky.script.expression.literal.type.FunctionType;
import com.github.thanospapapetrou.funcky.script.expression.literal.type.Type;

import java.net.URI;
import java.util.Objects;

import javax.script.ScriptContext;

/**
 * Class representing an application.
 * 
 * @author thanos
 */
public class Application extends Expression {
    private static final String APPLICATION_FORMAT = "%1$s %2$s";
    private static final String NESTED_APPLICATION = "(%1$s)";
    private static final String NULL_ARGUMENT = "Argument must not be null";
    private static final String NULL_CONTEXT = "Context must not be null";
    private static final String NULL_FUNCTION = "Function must not be null";

    private final Expression function;
    private final Expression argument;

    /**
     * Construct a new application.
     * 
     * @param engine
     *            the engine that compiled this application
     * @param script
     *            the URI of the script in which this application was encountered
     * @param line
     *            the line of the script at which this application was encountered
     * @param column
     *            the column of the script at which this application was encountered
     * @param function
     *            the function of this application
     * @param argument
     *            the argument of this application
     */
    public Application(final FunckyEngine engine, final URI script, final int line,
            final int column, final Expression function, final Expression argument) {
        super(engine, script, line, column);
        Objects.requireNonNull(function, NULL_FUNCTION);
        Objects.requireNonNull(argument, NULL_ARGUMENT);
        this.function = function;
        this.argument = argument;
    }

    /**
     * Construct a new application.
     * 
     * @param function
     *            the function of this application
     * @param argument
     *            the argument of this application
     */
    public Application(final Expression function, final Expression argument) {
        super();
        Objects.requireNonNull(function, NULL_FUNCTION);
        Objects.requireNonNull(argument, NULL_ARGUMENT);
        this.function = function;
        this.argument = argument;
    }

    Expression getFunction() {
        return function;
    }

    Expression getArgument() {
        return argument;
    }

    @Override
    public void check(final ScriptContext context)
            throws IllegalApplicationException, UndefinedReferenceException {
        Objects.requireNonNull(context, NULL_CONTEXT);
        function.check(context);
        argument.check(context);
        final Type functionType = function.getType(context);
        final Type argumentType = argument.getType(context);
        if (!((functionType instanceof FunctionType)
                && ((FunctionType) functionType).getDomain().equals(argumentType))) {
            throw new IllegalApplicationException(this, functionType, argumentType);
        }

    }

    @Override
    public boolean equals(final Object object) {
        if (object instanceof Application) {
            final Application application = (Application) object;
            return function.equals(application.function) && argument.equals(application.argument);
        }
        return false;
    }

    @Override
    public Literal eval(final ScriptContext context) {
        Objects.requireNonNull(context, NULL_CONTEXT);
        // TODO function may not be a function
        // TODO casting
        return ((Function) function.eval(context)).apply(context, argument);
    }

    @Override
    public Type getType(final ScriptContext context) {
        Objects.requireNonNull(context, NULL_CONTEXT);
        // TODO casting
        return ((FunctionType) function.getType(context)).getRange();
    }

    @Override
    public int hashCode() {
        return function.hashCode() + argument.hashCode();
    }

    @Override
    public String toString() {
        return String.format(APPLICATION_FORMAT, function, (argument instanceof Application)
                ? String.format(NESTED_APPLICATION, argument) : argument);
    }
}
