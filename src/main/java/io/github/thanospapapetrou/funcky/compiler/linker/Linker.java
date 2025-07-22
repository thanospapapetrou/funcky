package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyFactory;
import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.InvalidMainException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.NameAlreadyDefinedException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.PrefixAlreadyBoundException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.UndefinedMainException;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.prelude.Booleans;
import io.github.thanospapapetrou.funcky.runtime.prelude.Characters;
import io.github.thanospapapetrou.funcky.runtime.prelude.Combinators;
import io.github.thanospapapetrou.funcky.runtime.prelude.Commons;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import io.github.thanospapapetrou.funcky.runtime.prelude.Lists;
import io.github.thanospapapetrou.funcky.runtime.prelude.Numbers;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

public class Linker {
    public static final FunckyFunctionType MAIN_TYPE = new FunckyFunctionType(
            new FunckyListType(FunckyListType.STRING), FunckySimpleType.NUMBER);
    public static final URI STDIN;
    private static final String DEFINITION = "  %1$s %2$s";
    private static final String ERROR_LOADING_LIBRARY = "Error loading library %1$s";
    private static final String ERROR_RESOLVING_LIBRARY_NAMESPACE = "Error resolving library namespace";
    private static final String ERROR_RESOLVING_STDIN = "Error resolving standard input";
    private static final String ERROR_RESOLVING_USER_DIR = "Error resolving user directory";
    private static final Logger LOGGER = Logger.getLogger(Linker.class.getName());
    private static final Set<Class<? extends FunckyLibrary>> PRELUDE = Set.of(Types.class, Numbers.class,
            Booleans.class, Characters.class, Lists.class, Commons.class, Combinators.class);
    private static final String PRELUDE_SCHEME = new FunckyFactory().getLanguageName().toLowerCase(Locale.ROOT);
    private static final String PRELUDE_SCRIPT = "/prelude/%1$s.funcky";
    private static final URI USER_DIR;

    static {
        try {
            STDIN = new URI(PRELUDE_SCHEME, "stdin", null);
            USER_DIR = new File(System.getProperty("user.dir")).getCanonicalFile().toURI();
        } catch (final URISyntaxException e) {
            LOGGER.log(Level.SEVERE, ERROR_RESOLVING_STDIN, e);
            throw new ExceptionInInitializerError(e);
        } catch (final IOException e) {
            LOGGER.log(Level.SEVERE, ERROR_RESOLVING_USER_DIR, e);
            throw new ExceptionInInitializerError(e);
        }
    }

    private final FunckyEngine engine;

    public static URI getNamespace(final Class<? extends FunckyLibrary> library) {
        try {
            return new URI(PRELUDE_SCHEME, library.getSimpleName().toLowerCase(Locale.ROOT), null);
        } catch (final URISyntaxException e) {
            LOGGER.log(Level.SEVERE, ERROR_RESOLVING_LIBRARY_NAMESPACE, e);
            throw new IllegalStateException(ERROR_RESOLVING_LIBRARY_NAMESPACE, e);
        }
    }

    public Linker(final FunckyEngine engine) {
        this.engine = engine;
    }

    public FunckyExpression link(final FunckyExpression expression) throws CompilationException {
        engine.getContext().setLoaded(Linker.STDIN);
        if (expression != null) {
            LOGGER.fine(expression.getType().toString());
            LOGGER.fine("");
        }
        return expression;
    }

    public FunckyScript link(final FunckyScript script, final boolean main) throws CompilationException {
        validateImports(script);
        final Map<String, FunckyType> definitionTypes = validateDefinitions(script);
        if (main) {
            validateMain(script);
        }
        LOGGER.fine(script.getFile().toString());
        definitionTypes.forEach((definition, type) -> LOGGER.fine(String.format(DEFINITION, definition, type)));
        LOGGER.fine("");
        return script;
    }

    public InputStream getScript(final URI file) throws IOException {
        return ((getLibrary(file) != null)
                ? Linker.class.getResource(String.format(PRELUDE_SCRIPT, file.getSchemeSpecificPart()))
                : file.toURL()).openStream();
    }

    public URI normalize(final URI base, final URI namespace) {
        return namespace.isAbsolute() ? namespace : (base.equals(STDIN) ? USER_DIR : base).resolve(namespace);
    }

    private void validateImports(final FunckyScript script) throws PrefixAlreadyBoundException {
        for (final FunckyImport inport : script.getImports()) {
            final Optional<FunckyImport> otherImport = script.getImports().stream()
                    .filter(imp -> imp.getLine() < inport.getLine())
                    .filter(imp -> imp.getPrefix().equals(inport.getPrefix()))
                    .findFirst();
            if (otherImport.isPresent()) {
                throw new PrefixAlreadyBoundException(inport, otherImport.get());
            }
            engine.getContext().setImport(inport.getFile(), inport.getPrefix(),
                    normalize(script.getFile(), inport.getNamespace()));
        }
    }

    private Map<String, FunckyType> validateDefinitions(final FunckyScript script) throws CompilationException {
        final Map<String, FunckyType> definitionTypes = new LinkedHashMap<>();
        final Class<? extends FunckyLibrary> library = getLibrary(script.getFile());
        if (library != null) {
            script.getDefinitions().addAll(loadLibrary(library).getDefinitions());
        }
        for (final FunckyDefinition definition : script.getDefinitions()) {
            final Optional<FunckyDefinition> otherDefinition = script.getDefinitions().stream()
                    .filter(def -> def.getLine() < definition.getLine())
                    .filter(def -> def.getName().equals(definition.getName()))
                    .findFirst();
            if (otherDefinition.isPresent()) {
                throw new NameAlreadyDefinedException(definition, otherDefinition.get());
            }
            engine.getContext()
                    .setDefinitionExpression(definition.getFile(), definition.getName(), definition.getExpression());
        }
        engine.getContext().setLoaded(script.getFile());
        for (final FunckyDefinition definition : script.getDefinitions()) {
            definitionTypes.put(definition.getName(), definition.getExpression().getType());
        }
        return definitionTypes;
    }

    private void validateMain(final FunckyScript script) throws CompilationException {
        final Optional<FunckyDefinition> main = script.getDefinitions().stream()
                .filter(def -> def.getName().equals(FunckyScript.MAIN))
                .findAny();
        if (main.isEmpty()) {
            throw new UndefinedMainException(script);
        }
        final FunckyType mainType = main.get().getExpression().getType();
        try {
            if (mainType.unify(MAIN_TYPE) == null) {
                throw new InvalidMainException(main.get(), mainType);
            }
        } catch (final FunckyRuntimeException e) {
            throw new CompilationException(e);
        }
    }

    private Class<? extends FunckyLibrary> getLibrary(final URI file) {
        return PRELUDE.stream()
                .filter(library -> getNamespace(library).equals(file))
                .findFirst()
                .orElse(null);
    }

    private FunckyLibrary loadLibrary(final Class<? extends FunckyLibrary> library) {
        try {
            return library.getDeclaredConstructor().newInstance();
        } catch (final ReflectiveOperationException e) {
            LOGGER.log(Level.SEVERE, String.format(ERROR_LOADING_LIBRARY, getNamespace(library)), e);
            throw new ExceptionInInitializerError(e); // TOOD throw compilation exception instead
        }
    }
}
