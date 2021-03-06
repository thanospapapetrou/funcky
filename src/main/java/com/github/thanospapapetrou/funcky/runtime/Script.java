package com.github.thanospapapetrou.funcky.runtime;

import java.net.URI;
import java.util.List;
import java.util.Objects;

import javax.script.ScriptException;

import com.github.thanospapapetrou.funcky.FunckyScriptEngine;

/**
 * Class representing a Funcky script.
 * 
 * @author thanos
 */
public class Script extends AbstractSyntaxTreeNode {
	private static final String NULL_IMPORTS = "Imports must not be null";
	private static final String NULL_DEFINITIONS = "Definitions must not be null";

	/**
	 * The imports of this script.
	 */
	protected final List<Import> imports;

	/**
	 * The definitions of this script.
	 */
	protected final List<Definition> definitions;

	/**
	 * Construct a new script.
	 * 
	 * @param engine
	 *            the engine that generated this script
	 * @param script
	 *            the URI of this script
	 * @param line
	 *            the line from which this script was parsed
	 * @param imports
	 *            the imports of this script
	 * @param definitions
	 *            the definitions of this script
	 */
	public Script(final FunckyScriptEngine engine, final URI script, final int line, final List<Import> imports, final List<Definition> definitions) {
		super(engine, script, line);
		this.imports = Objects.requireNonNull(imports, NULL_IMPORTS);
		this.definitions = Objects.requireNonNull(definitions, NULL_DEFINITIONS);
	}

	@Override
	public Void eval() throws ScriptException {
		engine.createScope(script);
		for (final Import impord : imports) {
			impord.eval();
		}
		for (final Definition definition : definitions) {
			definition.eval();
		}
		return null;
	}

	/**
	 * Get the definitions.
	 * 
	 * @return the definitions of this script
	 */
	public List<Definition> getDefinitions() {
		return definitions;
	}

	/**
	 * Get the imports.
	 * 
	 * @return the imports of this script
	 */
	public List<Import> getImports() {
		return imports;
	}
}
