package com.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Logger;

import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

import com.github.thanospapapetrou.funcky.parser.Parser;
import com.github.thanospapapetrou.funcky.runtime.Script;
import com.github.thanospapapetrou.funcky.runtime.expressions.Application;
import com.github.thanospapapetrou.funcky.runtime.expressions.Expression;
import com.github.thanospapapetrou.funcky.runtime.expressions.Reference;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.Character;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.List;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.Literal;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.Number;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.Pair;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.types.FunctionType;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.types.ListType;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.types.PairType;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.types.Type;
import com.github.thanospapapetrou.funcky.runtime.expressions.literals.types.TypeVariable;
import com.github.thanospapapetrou.funcky.runtime.libraries.Booleans;
import com.github.thanospapapetrou.funcky.runtime.libraries.Characters;
import com.github.thanospapapetrou.funcky.runtime.libraries.Library;
import com.github.thanospapapetrou.funcky.runtime.libraries.Lists;
import com.github.thanospapapetrou.funcky.runtime.libraries.Numbers;
import com.github.thanospapapetrou.funcky.runtime.libraries.Pairs;
import com.github.thanospapapetrou.funcky.runtime.libraries.Prelude;
import com.github.thanospapapetrou.funcky.runtime.libraries.Strings;
import com.github.thanospapapetrou.funcky.runtime.libraries.UnknownBuiltInLibraryException;

/**
 * Class implementing a Funcky script engine.
 * 
 * @author thanos
 */
public class FunckyScriptEngine extends AbstractScriptEngine implements Compilable, Invocable {
	private static class Imports extends HashMap<String, URI> implements Map<String, URI> {
		private static final long serialVersionUID = 1L;

		private Imports() {
			super();
		}
	}

	@SuppressWarnings("unchecked")
	private static final Class<Library>[] BUILTIN_LIBRARIES = (Class<Library>[]) new Class<?>[] {Prelude.class, Booleans.class, Numbers.class, Characters.class, Pairs.class, Lists.class, Strings.class};
	private static final String EMPTY_NAME = "Name must not be empty";
	private static final String EMPTY_PREFIX = "Prefix must not be empty";
	private static final String IMPORTS = "%1$s.imports";
	private static final String IMPORTS_CAST_ERRROR = "Imports of script %1$s can not be cast to %2$s";
	private static final String LOADED = "Loaded %1$s";
	private static final String MAX_SCOPES = "Maximum number of scopes reached";
	private static final String NOT_INTEGER_SCOPE = "Scope of script %1$s is not an integer";
	private static final String NOT_LOADED_SCRIPT = "Script %1$s is not loaded";
	private static final String NULL_ARGUMENT = "Argument must not be null";
	private static final String NULL_CONTEXT = "Context must not be null";
	private static final String NULL_DOMAIN = "Domain must not be null";
	private static final String NULL_ELEMENT = "Element must not be null";
	private static final String NULL_EXPRESSION = "Expression must not be null";
	private static final String NULL_FACTORY = "Factory must not be null";
	private static final String NULL_FIRST = "First must not be null";
	private static final String NULL_FUNCTION = "Function must not be null";
	private static final String NULL_GLOBAL_SCOPE_BINDINGS = "Global scope bindings must not be null";
	private static final String NULL_HEAD = "Head must not be null";
	private static final String NULL_LIBRARY = "Library must not be null";
	private static final String NULL_NAME = "Name must not be null";
	private static final String NULL_PREFIX = "Prefix must not be null";
	private static final String NULL_RANGE = "Range must not be null";
	private static final String NULL_REFERENCE = "Reference must not be null";
	private static final String NULL_SCRIPT = "Script must not be null";
	private static final String NULL_SECOND = "Second must not be null";
	private static final String NULL_TAIL = "Tail must not be null";
	private static final String NULL_URI = "URI must not be null";
	private static final String RANDOM_NAME = "_%1$s%2$s";
	private static final String REFERENCE_RESOLUTION_ERROR = "Reference %1$s does not resolve to %2$s";
	private static final URI RUNTIME = URI.create("funcky:runtime");
	private static final String SCRIPT = "%1$s.script";
	private static final URI STDIN = URI.create("funcky:stdin");
	private static final String UNSUPPORTED_GET_INTERFACE = "getInterface() is not supported";
	private static final String UNSUPPORTED_INVOKE_FUNCTION = "invokeFunction() is not supported";
	private static final String UNSUPPORTED_INVOKE_METHOD = "invokeMethod() is not supported";

	private final FunckyScriptEngineFactory factory;
	private final Logger logger;

	/**
	 * Construct a new script engine.
	 * 
	 * @param factory
	 *            the script engine factory of this script engine
	 * @param globalScopeBindings
	 *            the global scope bindings of the initial context of this script engine
	 */
	public FunckyScriptEngine(final FunckyScriptEngineFactory factory, final Bindings globalScopeBindings) {
		this.factory = Objects.requireNonNull(factory, NULL_FACTORY);
		logger = Logger.getAnonymousLogger();
		logger.setUseParentHandlers(false);
		final Handler handler = new ConsoleHandler();
		handler.setFormatter(new ColorFormatter());
		logger.addHandler(handler);
		setContext(new FunckyScriptContext(Objects.requireNonNull(globalScopeBindings, NULL_GLOBAL_SCOPE_BINDINGS)));
	}

	private static String requireValidString(final String string, final String nullError, final String emptyError) {
		if (Objects.requireNonNull(string, nullError).isEmpty()) {
			throw new IllegalArgumentException(emptyError);
		}
		return string;
	}

	@Override
	public Script compile(final Reader script) throws ScriptException {
		try {
			return compile(Objects.requireNonNull(script, NULL_SCRIPT), Paths.get(context.getAttribute(ScriptEngine.FILENAME).toString()).toRealPath().toUri());
		} catch (final IOException e) {
			throw new ScriptException(e);
		}
	}

	@Override
	public Expression compile(final String script) throws ScriptException {
		createScope(STDIN);
		return compile(Objects.requireNonNull(script, NULL_SCRIPT), STDIN);
	}

	@Override
	public Bindings createBindings() {
		return new SimpleBindings();
	}

	/**
	 * Create a new scope for a script.
	 * 
	 * @param script
	 *            the URI of the script for which to create the new scope
	 * @throws ScriptException
	 *             if the new scope can not be created because the maximum number of scopes has been reached
	 */
	public void createScope(final URI script) throws ScriptException {
		Objects.requireNonNull(script, NULL_SCRIPT);
		for (int scope = Integer.MIN_VALUE; scope < Integer.MAX_VALUE; scope++) {
			if (!context.getScopes().contains(scope)) {
				context.setAttribute(script.toString(), scope, ScriptContext.ENGINE_SCOPE);
				context.setAttribute(String.format(SCRIPT, getFactory().getExtensions().get(0)), script, scope);
				context.setAttribute(String.format(IMPORTS, getFactory().getExtensions().get(0)), new Imports(), scope);
				return;
			}
		}
		throw new ScriptException(MAX_SCOPES);
	}

	/**
	 * Declare an import.
	 * 
	 * @param script
	 *            the URI of the script in which to declare the import
	 * @param prefix
	 *            the prefix of the import to declare
	 * @param uri
	 *            the URI of the import to declare
	 * @throws ScriptException
	 *             if any errors occur while declaring this import
	 */
	public void declare(final URI script, final String prefix, final URI uri) throws ScriptException {
		final Map<String, URI> imports = getImports(Objects.requireNonNull(script, NULL_SCRIPT));
		imports.put(requireValidString(prefix, NULL_PREFIX, EMPTY_PREFIX), Objects.requireNonNull(uri, NULL_URI));
	}

	/**
	 * Define a definition.
	 * 
	 * @param script
	 *            the URI of the script in which to define the definition
	 * @param name
	 *            the name of the definition to define
	 * @param expression
	 *            the expression of the definition to define
	 * @throws ScriptException
	 *             if any errors occur while defining this definition
	 */
	public void define(final URI script, final String name, final Expression expression) throws ScriptException {
		final Integer scope = getScope(Objects.requireNonNull(script, NULL_SCRIPT));
		if (scope == null) {
			illegalState(String.format(NOT_LOADED_SCRIPT, script));
		}
		context.setAttribute(requireValidString(name, NULL_NAME, EMPTY_NAME), Objects.requireNonNull(expression, NULL_EXPRESSION), scope);
	}

	@Override
	public Void eval(final Reader script, final ScriptContext context) throws ScriptException {
		try {
			compile(Objects.requireNonNull(script, NULL_SCRIPT), Paths.get(Objects.requireNonNull(context, NULL_CONTEXT).getAttribute(ScriptEngine.FILENAME).toString()).toRealPath().toUri()).eval();
			return null;
		} catch (final IOException e) {
			throw new ScriptException(e);
		}
	}

	@Override
	public Literal eval(final String script, final ScriptContext ignored) {
		try {
			createScope(STDIN);
			return compile(Objects.requireNonNull(script, NULL_SCRIPT), STDIN).eval();
		} catch (final ScriptException e) {
			logger.warning(e.getMessage());
			return null;
		}
	}

	/**
	 * Get a new application generated at runtime.
	 * 
	 * @param function
	 *            the function of the application
	 * @param argument
	 *            the argument of the application
	 * @return a new application
	 */
	public Application getApplication(final Expression function, final Expression argument) {
		return new Application(this, RUNTIME, -1, Objects.requireNonNull(function, NULL_FUNCTION), Objects.requireNonNull(argument, NULL_ARGUMENT));
	}

	/**
	 * Get a new character generated at runtime.
	 * 
	 * @param value
	 *            the value of the character
	 * @return a new character
	 */
	public Character getCharacter(char value) {
		return new Character(this, RUNTIME, -1, value);
	}

	@Override
	public FunckyScriptEngineFactory getFactory() {
		return factory;
	}

	/**
	 * Get a new function type generated at runtime.
	 * 
	 * @param domain
	 *            the domain type of the function type
	 * @param range
	 *            the range type of the function type
	 * @return a new function type
	 */
	public FunctionType getFunctionType(final Type domain, final Type range) {
		return new FunctionType(this, RUNTIME, Objects.requireNonNull(domain, NULL_DOMAIN), Objects.requireNonNull(range, NULL_RANGE));
	}

	@Override
	public <T> T getInterface(final Class<T> clazz) {
		throw new UnsupportedOperationException(UNSUPPORTED_GET_INTERFACE);
	}

	@Override
	public <T> T getInterface(final Object object, final Class<T> clazz) {
		throw new UnsupportedOperationException(UNSUPPORTED_GET_INTERFACE);
	}

	/**
	 * Get a new list generated at runtime.
	 * 
	 * @param head
	 *            the head of the list
	 * @param tail
	 *            the tail of the list
	 * @return a new list
	 * @throws ScriptException
	 *             if any errors occur while generating the list
	 */
	public List getList(final Literal head, final List tail) throws ScriptException {
		return new List(this, RUNTIME, -1, Objects.requireNonNull(head, NULL_HEAD), Objects.requireNonNull(tail, NULL_TAIL));
	}

	/**
	 * Get a new empty list generated at runtime.
	 * 
	 * @param element
	 *            the element type of the type of the list
	 * @return a new empty list
	 */
	public List getList(final Type element) {
		return new List(this, RUNTIME, -1, getListType(element));
	}

	/**
	 * Get a new list type generated at runtime.
	 * 
	 * @param element
	 *            the element type of the list type
	 * @return a new list type
	 */
	public ListType getListType(final Type element) {
		return new ListType(this, RUNTIME, Objects.requireNonNull(element, NULL_ELEMENT));
	}

	/**
	 * Get a new number generated at runtime.
	 * 
	 * @param value
	 *            the value of the number
	 * @return a new number
	 */
	public Number getNumber(final double value) {
		return new Number(this, RUNTIME, -1, value);
	}

	/**
	 * Get a new pair generated at runtime.
	 * 
	 * @param first
	 *            the first value of the pair
	 * @param second
	 *            the second value of the pair
	 * @return a new pair
	 */
	public Pair getPair(final Literal first, final Literal second) {
		return new Pair(this, RUNTIME, -1, Objects.requireNonNull(first, NULL_FIRST), Objects.requireNonNull(second, NULL_SECOND));
	}

	/**
	 * Get a new pair type generated at runtime.
	 * 
	 * @param first
	 *            the first type of the pair type
	 * @param second
	 *            the second type of the pair type
	 * @return a new pair type
	 */
	public PairType getPairType(final Type first, final Type second) {
		return new PairType(this, RUNTIME, Objects.requireNonNull(first, NULL_FIRST), Objects.requireNonNull(second, NULL_SECOND));
	}

	/**
	 * Get a new reference generated at runtime.
	 * 
	 * @param uri
	 *            the URI of the reference
	 * @param name
	 *            the name of the reference
	 * @return a new reference
	 */
	public Reference getReference(final URI uri, final String name) {
		return new Reference(this, RUNTIME, -1, Objects.requireNonNull(uri, NULL_URI), requireValidString(name, NULL_NAME, EMPTY_NAME));
	}

	/**
	 * Get a new reference generated at runtime.
	 * 
	 * @param library
	 *            the library of the reference
	 * @param name
	 *            the name of the reference
	 * @return a new reference
	 */
	public Reference getReference(final Class<? extends Library> library, final String name) {
		return getReference(Library.getUri(Objects.requireNonNull(library, NULL_LIBRARY)), requireValidString(name, NULL_NAME, EMPTY_NAME));
	}

	/**
	 * Get the scope of a script.
	 * 
	 * @param script
	 *            the URI of the script whose scope to retrieve
	 * @return the scope of the given script or <code>null</code> if the given script is not loaded
	 * @throws ScriptException
	 *             if any errors occur while retrieving scope
	 */
	public Integer getScope(final URI script) throws ScriptException {
		final Object scope = context.getAttribute(Objects.requireNonNull(script, NULL_SCRIPT).toString(), ScriptContext.ENGINE_SCOPE);
		return ((scope == null) || (scope instanceof Integer)) ? (Integer) scope : this.<Integer> illegalState(String.format(NOT_INTEGER_SCOPE, script));
	}

	/**
	 * Get a new type variable generated at runtime.
	 * 
	 * @return a new type variable
	 */
	public TypeVariable getTypeVariable() {
		final UUID uuid = UUID.randomUUID();
		return new TypeVariable(this, RUNTIME, -1, String.format(RANDOM_NAME, Long.toHexString(uuid.getMostSignificantBits()), Long.toHexString(uuid.getLeastSignificantBits())));
	}

	@Override
	public Object invokeFunction(final String function, final Object... arguments) {
		throw new UnsupportedOperationException(UNSUPPORTED_INVOKE_FUNCTION);
	}

	@Override
	public Object invokeMethod(final Object object, final String method, final Object... arguments) {
		throw new UnsupportedOperationException(UNSUPPORTED_INVOKE_METHOD);
	}

	/**
	 * Load a script or a built-in library.
	 * 
	 * @param reference
	 *            the reference that refers to the script or built-in library to load
	 * @throws ScriptException
	 *             if any errors occur while loading the script or the built-in library
	 */
	public void load(final Reference reference) throws ScriptException {
		if (Objects.requireNonNull(reference, NULL_REFERENCE).getUri().getScheme().equals(getFactory().getExtensions().get(0))) {
			loadLibrary(reference);
		} else {
			try {
				loadScript(reference.getUri().toURL());
			} catch (final MalformedURLException e) {
				throw new ScriptException(e);
			}
		}
	}

	/**
	 * Resolve a reference.
	 * 
	 * @param reference
	 *            the reference to resolve
	 * @return the expression corresponding to the given reference or <code>null</code> if the given reference is not defined
	 * @throws ScriptException
	 *             if any errors occur while resolving reference
	 */
	public Expression resolve(final Reference reference) throws ScriptException {
		final Integer scope = getScope(Objects.requireNonNull(reference, NULL_REFERENCE).getUri());
		if (scope == null) {
			return illegalState(String.format(NOT_LOADED_SCRIPT, reference.getUri()));
		}
		final Object expression = context.getAttribute(Objects.requireNonNull(reference, NULL_REFERENCE).getName(), scope);
		return ((expression == null) || (expression instanceof Expression)) ? (Expression) expression : this.<Expression> illegalState(String.format(REFERENCE_RESOLUTION_ERROR, reference, Expression.class.getName()));
	}

	/**
	 * Resolve a prefix.
	 * 
	 * @param script
	 *            the URI of the script in which to search
	 * @param prefix
	 *            the prefix to resolve
	 * @return the URI corresponding to the given prefix in the given script or <code>null</code> if the given prefix is not declared in the given script
	 * @throws ScriptException
	 *             if any errors occur while resolving prefix
	 */
	public URI resolve(final URI script, final String prefix) throws ScriptException {
		return getImports(Objects.requireNonNull(script, NULL_SCRIPT)).get(prefix);
	}

	private Script compile(final Reader script, final URI scriptUri) throws ScriptException {
		return new Parser(this, script, scriptUri).parseScript();
	}

	private Expression compile(final String script, final URI scriptUri) throws ScriptException {
		return new Parser(this, new StringReader(script), scriptUri).parseExpression();
	}

	private Map<String, URI> getImports(final URI script) throws ScriptException {
		final Integer scope = getScope(script);
		if (scope == null) {
			return illegalState(String.format(NOT_LOADED_SCRIPT, script));
		}
		final Object imports = context.getAttribute(String.format(IMPORTS, getFactory().getExtensions().get(0)), scope);
		return (imports instanceof Imports) ? (Imports) imports : this.<Map<String, URI>> illegalState(String.format(IMPORTS_CAST_ERRROR, script, Imports.class.getName()));
	}

	private <T> T illegalState(final String message) throws ScriptException {
		throw new ScriptException(new IllegalStateException(message));
	}

	private void loadLibrary(final Reference reference) throws ScriptException {
		for (final Class<Library> library : BUILTIN_LIBRARIES) {
			if (Library.getUri(library).equals(reference.getUri())) {
				try {
					library.getConstructor(FunckyScriptEngine.class).newInstance(this).eval(context);
					logger.info(String.format(LOADED, reference.getUri()));
					return;
				} catch (final IllegalAccessException | InstantiationException | InvocationTargetException | NoSuchMethodException e) {
					throw new ScriptException(e);
				}
			}
		}
		throw new UnknownBuiltInLibraryException(reference);
	}

	private void loadScript(final URL script) throws ScriptException {
		try (final InputStreamReader reader = new InputStreamReader(script.openStream(), StandardCharsets.UTF_8)) {
			compile(reader, script.toURI()).eval(context);
			logger.info(String.format(LOADED, script));
		} catch (final IOException | URISyntaxException e) {
			throw new ScriptException(e);
		}
	}
}
