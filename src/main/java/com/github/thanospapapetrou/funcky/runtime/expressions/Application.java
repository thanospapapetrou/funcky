package com.github.thanospapapetrou.funcky.runtime.expressions;

import java.net.URI;
import java.util.Objects;

import javax.script.ScriptException;

import com.github.thanospapapetrou.funcky.FunckyScriptEngine;
import com.github.thanospapapetrou.funcky.runtime.exceptions.InvalidArgumentException;
import com.github.thanospapapetrou.funcky.runtime.exceptions.InvalidFunctionException;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.Function;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.Literal;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.types.FunctionType;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.types.Type;

/**
 * Class representing a Funcky application.
 * 
 * @author thanos
 */
public class Application extends Expression {
	private static final String APPLICATION = "%1$s %2$s";
	private static final String NESTED_APPLICATION = "(%1$s)";
	private static final String NULL_FUNCTION = "Function must not be null";
	private static final String NULL_ARGUMENT = "Argument must not be null";

	private final Expression function;
	private final Expression argument;

	/**
	 * Construct a new application.
	 * 
	 * @param engine
	 *            the engine that generated this application
	 * @param script
	 *            the URI of the script from which this application was generated
	 * @param line
	 *            the line from which this application was parsed or <code>-1</code> if this application was not parsed (is built-in or generated at runtime)
	 * @param function
	 *            the function of this application
	 * @param argument
	 *            the argument of this application
	 */
	public Application(final FunckyScriptEngine engine, final URI script, final int line, final Expression function, final Expression argument) {
		super(engine, script, line);
		this.function = Objects.requireNonNull(function, NULL_FUNCTION);
		this.argument = Objects.requireNonNull(argument, NULL_ARGUMENT);
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
	public <L extends Literal> L evaluate(final Class<L> clazz) throws ScriptException {
		super.evaluate(clazz);
		checkTypes();
		return function.evaluate(Function.class).apply(argument).evaluate(clazz);
	}

	/**
	 * Get the argument of this application.
	 * 
	 * @return the argument of this application
	 */
	public Expression getArgument() {
		return argument;
	}

	/**
	 * Get the function of this application.
	 * 
	 * @return the function of this application
	 */
	public Expression getFunction() {
		return function;
	}

	@Override
	public Type getType() throws ScriptException {
		checkTypes();
		final FunctionType functionType = (FunctionType) function.getType(); // TODO no casting
		return functionType.getRange().bind(functionType.getDomain().infer(argument.getType().free()));
	}

	@Override
	public int hashCode() {
		return Objects.hash(function, argument);
	}

	@Override
	public String toString() {
		final Expression argumentExpression = (argument instanceof Literal) ? ((Literal) argument).toExpression() : argument;
		return String.format(APPLICATION, function, (argumentExpression instanceof Application) ? String.format(NESTED_APPLICATION, argumentExpression) : argumentExpression);
	}

	private void checkTypes() throws ScriptException {
		if (!(function.getType() instanceof FunctionType)) {
			throw new InvalidFunctionException(function);
		}
		if (((FunctionType) function.getType()).getDomain().infer(argument.getType().free()) == null) { // TODO no casting
			throw new InvalidArgumentException(this);
		}
	}
}
