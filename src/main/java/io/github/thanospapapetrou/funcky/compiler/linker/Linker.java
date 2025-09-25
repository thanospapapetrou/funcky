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
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.logging.Logger;

import io.github.thanospapapetrou.funcky.FunckyEngine;
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
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;

public class Linker {
    public static final String JAVA_PREFIX = "$"; // TODO move to transpiler
    public static final Function<FunckyEngine, FunckyFunctionType> MAIN_TYPE = engine ->
            new FunckyFunctionType(engine, new FunckyListType(engine, FunckyListType.STRING.apply(engine)),
                    FunckySimpleType.NUMBER.apply(engine));

    private static final String DEFINITION = "  %1$s%n    %2$s";
    private static final String ERROR_LOADING_LIBRARY = "Error loading library %1$s";
    private static final String ERROR_NORMALISING_NAMESPACE = "Error normalising namespace %1$s";
    private static final String ERROR_RESOLVING_NAMESPACE = "Error resolving namespace for library `%1$s`";
    private static final String ERROR_RESOLVING_STDIN = "Error resolving stdin";
    private static final Logger LOGGER = Logger.getLogger(Linker.class.getName());
    private static final String PRELUDE_SCRIPT = "/prelude/%1$s.%2$s";
    private static final String STDIN = "stdin";
    private static final String USER_DIR = "user.dir";

    private final FunckyEngine engine;

    public Linker(final FunckyEngine engine) {
        this.engine = engine;
    }

    public URI getNamespace(final Class<? extends FunckyLibrary> library) {
        try {
            return new URI(getPreludeScheme(), library.getSimpleName().toLowerCase(Locale.ROOT), null);
        } catch (final URISyntaxException e) {
            throw new IllegalStateException(String.format(ERROR_RESOLVING_NAMESPACE, library.getName()), e);
        }
    }

    public URI getStdin() {
        try {
            return new URI(getPreludeScheme(), STDIN, null);
        } catch (final URISyntaxException e) {
            throw new IllegalStateException(ERROR_RESOLVING_STDIN, e);
        }
    }

    public URI normalize(final URI base, final URI namespace) {
        try {
            return namespace.isAbsolute() ? namespace
                    : (base.equals(getStdin()) ? new File(System.getProperty(USER_DIR)).getCanonicalFile().toURI()
                            : base).resolve(namespace);
        } catch (final IOException e) {
            throw new IllegalStateException(String.format(ERROR_NORMALISING_NAMESPACE, namespace), e);
        }
    }

    public FunckyExpression link(final FunckyExpression expression) {
        engine.getManager().setLoaded(getStdin());
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
        return Objects.requireNonNull((getLibrary(file) != null) ? Linker.class.getResource(
                String.format(PRELUDE_SCRIPT, file.getSchemeSpecificPart(),
                        engine.getFactory().getExtensions().getFirst())) : file.toURL()).openStream();
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
            engine.getManager().setImport(inport, normalize(inport.file(), inport.namespace()));
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

    @SuppressWarnings("unchecked")
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

    private String getPreludeScheme() {
        return engine.getFactory().getLanguageName().toLowerCase(Locale.ROOT);
    }
}
