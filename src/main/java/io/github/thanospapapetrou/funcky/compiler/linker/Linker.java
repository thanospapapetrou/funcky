package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.logging.Logger;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyFactory;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.exceptions.InvalidMainException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.NameAlreadyDefinedException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.PrefixAlreadyBoundException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UndefinedMainException;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.prelude.Booleans;
import io.github.thanospapapetrou.funcky.runtime.prelude.Characters;
import io.github.thanospapapetrou.funcky.runtime.prelude.Combinators;
import io.github.thanospapapetrou.funcky.runtime.prelude.Commons;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import io.github.thanospapapetrou.funcky.runtime.prelude.Lists;
import io.github.thanospapapetrou.funcky.runtime.prelude.Numbers;
import io.github.thanospapapetrou.funcky.runtime.prelude.Types;

public class Linker {
    public static final String JAVA_PREFIX = "$"; // TODO move to transpiler
    public static final Function<FunckyEngine, FunckyFunctionType> MAIN_TYPE = engine ->
            new FunckyFunctionType(engine, new FunckyListType(engine, FunckyListType.STRING.apply(engine)),
                    FunckySimpleType.NUMBER.apply(engine));
    public static final URI STDIN;

    private static final String DEFINITION = "  %1$s%n    %2$s";
    private static final String ERROR_LOADING_LIBRARY = "Error loading library %1$s";
    private static final String ERROR_RESOLVING_LIBRARY_NAMESPACE = "Error resolving library namespace";
    private static final Logger LOGGER = Logger.getLogger(Linker.class.getName());
    private static final String PRELUDE_SCHEME = FunckyFactory.getParameters(FunckyEngine.LANGUAGE).get(0)
            .toLowerCase(Locale.ROOT);
    private static final String PRELUDE_SCRIPT = "/prelude/%1$s.funcky"; // TODO use extension
    private static final URI USER_DIR;

    static {
        try {
            STDIN = new URI(PRELUDE_SCHEME, "stdin", null);
            USER_DIR = new File(System.getProperty("user.dir")).getCanonicalFile().toURI();
        } catch (final IOException | URISyntaxException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private final FunckyEngine engine;

    public static URI normalize(final URI base, final URI namespace) {
        return namespace.isAbsolute() ? namespace : (base.equals(STDIN) ? USER_DIR : base).resolve(namespace);
    }

    public static URI getNamespace(final Class<? extends FunckyLibrary> library) {
        try {
            return new URI(PRELUDE_SCHEME, library.getSimpleName().toLowerCase(Locale.ROOT), null);
        } catch (final URISyntaxException e) {
            throw new IllegalStateException(ERROR_RESOLVING_LIBRARY_NAMESPACE, e);
        }
    }

    public Linker(final FunckyEngine engine) {
        this.engine = engine;
    }

    public FunckyExpression link(final FunckyExpression expression) {
        engine.getManager().setLoaded(Linker.STDIN);
        if (expression != null) {
            LOGGER.fine(expression.getType().toString());
        }
        return expression;
    }

    public FunckyScript link(final FunckyScript script, final boolean main) {
        validateImports(script);
        final Map<String, FunckyType> definitionTypes = validateDefinitions(script);
        if (main) {
            validateMain(script);
        }
        LOGGER.fine(script.getFile().toString());
        definitionTypes.entrySet().stream()
                .map(definitionType -> String.format(DEFINITION, definitionType.getKey(), definitionType.getValue()))
                .forEach(LOGGER::fine);
        return script;
    }

    public InputStream getScript(final URI file) throws IOException {
        return ((getLibrary(file) != null) ? Linker.class.getResource(String.format(PRELUDE_SCRIPT,
                file.getSchemeSpecificPart())) : file.toURL()).openStream();
    }

    private void validateImports(final FunckyScript script) {
        for (final FunckyImport inport : script.getImports()) {
            final Optional<FunckyImport> otherImport = script.getImports().stream()
                    .filter(imp -> imp.line() < inport.line())
                    .filter(imp -> imp.prefix().equals(inport.prefix()))
                    .findFirst();
            if (otherImport.isPresent()) {
                throw new SneakyCompilationException(new PrefixAlreadyBoundException(inport, otherImport.get()));
            }
            engine.getManager().setImport(inport.file(), inport.prefix(), normalize(script.getFile(),
                    inport.namespace()));
        }
    }

    private Map<String, FunckyType> validateDefinitions(final FunckyScript script) {
        final Map<String, FunckyType> definitionTypes = new LinkedHashMap<>();
        final Class<? extends FunckyLibrary> library = getLibrary(script.getFile());
        if (library != null) {
            script.getDefinitions().addAll(loadLibrary(library).getDefinitions());
        }
        for (final FunckyDefinition definition : script.getDefinitions()) {
            final Optional<FunckyDefinition> otherDefinition = script.getDefinitions().stream()
                    .filter(def -> def.line() < definition.line())
                    .filter(def -> def.name().equals(definition.name()))
                    .findFirst();
            if (otherDefinition.isPresent()) {
                throw new SneakyCompilationException(
                        new NameAlreadyDefinedException(definition, otherDefinition.get()));
            }
            engine.getManager().setDefinitionExpression(definition);
        }
        engine.getManager().setLoaded(script.getFile());
        for (final FunckyDefinition definition : script.getDefinitions()) {
            definitionTypes.put(definition.name(), definition.expression().getType());
        }
        return definitionTypes;
    }

    private void validateMain(final FunckyScript script) {
        final Optional<FunckyDefinition> main = script.getDefinitions().stream()
                .filter(def -> def.name().equals(FunckyScript.MAIN))
                .findAny();
        if (main.isEmpty()) {
            throw new SneakyCompilationException(new UndefinedMainException(script));
        }
        if (main.get().expression().getType().unify(MAIN_TYPE.apply(engine)) == null) {
            throw new SneakyCompilationException(new InvalidMainException(main.get()));
        }
    }

    private Class<? extends FunckyLibrary> getLibrary(final URI file) {
        return (Class<? extends FunckyLibrary>) Arrays.stream(FunckyLibrary.class.getPermittedSubclasses())
                .filter(library -> getNamespace((Class<? extends FunckyLibrary>) library).equals(file))
                .findFirst()
                .orElse(null);
    }

    private FunckyLibrary loadLibrary(final Class<? extends FunckyLibrary> library) {
        try {
            return library.getDeclaredConstructor(FunckyEngine.class).newInstance(engine);
        } catch (final ReflectiveOperationException e) {
            throw new IllegalStateException(String.format(ERROR_LOADING_LIBRARY, getNamespace(library)), e);
        }
    }
}
