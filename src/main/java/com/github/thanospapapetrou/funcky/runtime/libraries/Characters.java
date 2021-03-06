package com.github.thanospapapetrou.funcky.runtime.libraries;

import java.net.URI;

import javax.script.ScriptException;

import com.github.thanospapapetrou.funcky.FunckyScriptEngine;
import com.github.thanospapapetrou.funcky.runtime.expressions.Expression;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.ApplicableFunction;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.Character;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.Literal;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.Number;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.functors.ApplicableFunctor;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.types.SimpleType;

/**
 * Characters related library.
 * 
 * @author thanos
 */
public class Characters extends Library {
	/**
	 * Type of characters.
	 */
	public static final String CHARACTER = "character";

	/**
	 * Get the code point of a surrogate pair.
	 */
	public static final String CODE_POINT = "codePoint";

	/**
	 * Get the high surrogate of a code point.
	 */
	public static final String HIGH_SURROGATE = "highSurrogate";

	/**
	 * Get the low surrogate of a code point.
	 */
	public static final String LOW_SURROGATE = "lowSurrogate";

	private static final URI CHARACTERS = Library.getUri(Characters.class);

	/**
	 * Construct a new characters library.
	 * 
	 * @param engine
	 *            the engine constructing this characters library
	 * @throws ScriptException
	 *             if any errors occur while constructing this characters library
	 */
	public Characters(final FunckyScriptEngine engine) throws ScriptException {
		super(engine);
		final SimpleType characterType = new SimpleType(engine, CHARACTERS, CHARACTER);
		addDefinition(characterType);
		final SimpleType numberType = engine.getReference(Numbers.class, Numbers.NUMBER).evaluate(SimpleType.class);
		addFunctorDefinition(CODE_POINT, new ApplicableFunctor() {
			@Override
			public Number apply(final Expression... arguments) throws ScriptException {
				return engine.getNumber(java.lang.Character.toCodePoint(arguments[0].evaluate(Character.class).getValue(), arguments[1].evaluate(Character.class).getValue()));
			}
		}, characterType, characterType, numberType);
		// TODO check that argument is int
		addFunctionDefinition(HIGH_SURROGATE, numberType, characterType, new ApplicableFunction() {
			@Override
			public Literal apply(final Expression argument) throws ScriptException {
				return engine.getCharacter(java.lang.Character.highSurrogate(Double.valueOf(argument.evaluate(Number.class).getValue()).intValue()));
			}
		});
		// TODO check that argument is int
		addFunctionDefinition(LOW_SURROGATE, numberType, characterType, new ApplicableFunction() {
			@Override
			public Literal apply(final Expression argument) throws ScriptException {
				return engine.getCharacter(java.lang.Character.lowSurrogate(Double.valueOf(argument.evaluate(Number.class).getValue()).intValue()));
			}
		});
		// TODO
		// Character.charCount(codePoint);
		// Character.digit(codePoint, radix);
		// Character.forDigit(digit, radix);
		// Character.getDirectionality(codePoint);
		// Character.getType(codePoint);
		// Character.getName(codePoint);
		// Character.getNumericValue(codePoint);
		// Character.is...
		// Character.isDefined(codePoint)
		// Character.isValidCodePoint(codePoint)
		// Character.isBmpCodePoint(codePoint)
		// Character.isHighSurrogate(ch)
		// Character.isLowSurrogate(ch)
		// Character.isMirrored(codePoint)
		// Character.isSupplementaryCodePoint(codePoint)
		// Character.toChars(codePoint)
		// Character.toLowerCase(codePoint)
		// Character.toTitleCase(codePoint)
		// Character.toUpperCase(codePoint)
	}
}
