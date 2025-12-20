package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.logging.Logger;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.InvalidMainException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.NameAlreadyDefinedException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.PrefixAlreadyBoundException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.UndefinedMainException;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import io.github.thanospapapetrou.funcky.runtime.prelude.HigherOrderFunction;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER;

public class Linker {
    public static final Function<FunckyEngine, FunckyFunctionType> MAIN_TYPE = FUNCTION(LIST(STRING), NUMBER);

    private static final String DEFINITION = "  %1$s%n    %2$s";
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
        engine.getContext().setScript(getStdin());
        if (expression != null) {
            LOGGER.fine(expression.getType().toString());
        }
        return expression;
    }

    public FunckyScript link(final FunckyScript script, final boolean main) {
        final FunckyScript normalized = normalize(script);
        final Map<String, FunckyType> definitionTypes = validateDefinitions(normalized);
        if (main) {
            validateMain(normalized);
        }
        LOGGER.fine(normalized.getFile().toString());
        definitionTypes.entrySet().stream()
                .map(definitionType -> String.format(DEFINITION, definitionType.getKey(), definitionType.getValue()))
                .forEach(LOGGER::fine);
        return normalized;
    }

    public InputStream getScript(final URI file) throws IOException {
        return Objects.requireNonNull((getLibrary(file) != null) ? Linker.class.getResource(
                String.format(PRELUDE_SCRIPT, file.getSchemeSpecificPart(),
                        engine.getFactory().getExtensions().getFirst())) : file.toURL()).openStream();
    }

    private Map<String, FunckyType> validateDefinitions(final FunckyScript script) {
        final Map<String, FunckyType> definitionTypes = new LinkedHashMap<>();
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

    private String getPreludeScheme() {
        return engine.getFactory().getLanguageName().toLowerCase(Locale.ROOT);
    }

    private FunckyScript normalize(final FunckyScript script) {
        final FunckyScript normalized = new FunckyScript(engine, script.getFile());
        engine.getContext().setScript(normalized.getFile());
        script.getImports().stream()
                .map(inport -> normalize(inport, script.getImports()))
                .forEach(normalized.getImports()::add);
        script.getDefinitions().stream()
                .map(definition -> normalize(definition, script.getDefinitions()))
                .forEach(normalized.getDefinitions()::add);
        return normalized;
    }

    private FunckyImport normalize(final FunckyImport inport, final List<FunckyImport> others) {
        final Optional<FunckyImport> other = others.stream()
                .filter(imp -> imp.line() < inport.line())
                .filter(imp -> imp.prefix().equals(inport.prefix()))
                .findFirst();
        if (other.isPresent()) {
            throw new SneakyCompilationException(new PrefixAlreadyBoundException(inport, other.get()));
        }
        final FunckyImport normalized = new FunckyImport(inport.file(), inport.line(), inport.prefix(),
                normalize(inport.file(), inport.namespace()));
        engine.getContext().setImport(inport);
        return normalized;
    }

    private FunckyDefinition normalize(final FunckyDefinition definition, final List<FunckyDefinition> others) {
        final Optional<FunckyDefinition> other = others.stream()
                .filter(def -> def.line() < definition.line())
                .filter(def -> def.name().equals(definition.name()))
                .findFirst();
        if (other.isPresent()) {
            throw new SneakyCompilationException(new NameAlreadyDefinedException(definition, other.get()));
        }
        FunckyDefinition normalized = null; // TODO improve
        if ((definition.expression() instanceof FunckyReference reference) && (reference.getNamespace() != null)
                && reference.getNamespace().getScheme().equals("java")) { // TODO
            try {
                final Class<?> clazz = Class.forName(reference.getNamespace().getSchemeSpecificPart());
                final Field field = clazz.getDeclaredField(reference.getName());
                final Constructor<?> constructor = clazz.getDeclaredConstructor(FunckyEngine.class);
                if (FunckyValue.class.isAssignableFrom(field.getType())) {
                    if (HigherOrderFunction.class.isAssignableFrom(field.getType())) {
                        final HigherOrderFunction function =
                                (HigherOrderFunction) field.get(constructor.newInstance(engine));
                        normalized = new FunckyDefinition(definition.file(),
                                definition.line(), definition.name(), new FunckyLiteral(engine,
                                reference.getFile(), reference.getLine(), reference.getColumn(),
                                new HigherOrderFunction(engine, function.getType(), function.getOrder(),
                                        new FunckyReference(engine, definition.file(), definition.name())) {
                                    @Override
                                    public FunckyValue apply(final ScriptContext context,
                                            final List<FunckyExpression> arguments) {
                                        return function.apply(context, arguments);
                                    }
                                }));
                    } else {
                        normalized = new FunckyDefinition(definition.file(),
                                definition.line(), definition.name(), new FunckyLiteral(engine,
                                reference.getFile(), reference.getLine(), reference.getColumn(),
                                (FunckyValue) field.get(constructor.newInstance(engine))));
                    }
                }
            } catch (final ClassNotFoundException | NoSuchFieldException | InstantiationException |
                    IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
        } else {
            normalized = new FunckyDefinition(definition.file(), definition.line(), definition.name(),
                    definition.expression()); // TODO
        }
        engine.getContext().setDefinition(normalized);
        return normalized;
    }
}
