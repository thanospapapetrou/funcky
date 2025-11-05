package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.logging.Logger;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyFactory;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.exceptions.IllegalApplicationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.InvalidMainException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.NameAlreadyDefinedException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.PrefixAlreadyBoundException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnboundPrefixException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UndefinedMainException;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;

import static io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType.FUNCTION;
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.LIST;
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.STRING;
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.NUMBER;

public class Linker {
    public static final FunckyFunctionType MAIN_TYPE;
    public static final URI STDIN;

    private static final String DEFINITION = "  %1$s%n    %2$s";
    private static final String ERROR_LOADING_LIBRARY = "Error loading library %1$s";
    private static final Logger LOGGER = Logger.getLogger(Linker.class.getName());
    private static final Map<Class<? extends FunckyLibrary>, URI> PRELUDE = new HashMap<>();
    private static final String PRELUDE_SCRIPT = "/prelude/%1$s.%2$s";

    private final FunckyEngine engine;
    private final URI base;

    static {
        try {
            for (final Class<?> library : FunckyLibrary.class.getPermittedSubclasses()) {
                PRELUDE.put((Class<? extends FunckyLibrary>) library,
                        new URI(FunckyFactory.getParameters(FunckyEngine.LANGUAGE).getFirst()
                        .toLowerCase(Locale.ROOT), library.getSimpleName().toLowerCase(Locale.ROOT),
                        null));
            }
            MAIN_TYPE = FUNCTION(LIST(STRING), NUMBER); // this has to be initialized after PRELUDE
            STDIN = new URI(FunckyFactory.getParameters(FunckyEngine.LANGUAGE).getFirst().toLowerCase(Locale.ROOT),
                    "stdin", null);
        } catch (final URISyntaxException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static URI getNamespace(final Class<? extends FunckyScript> library) {
        return PRELUDE.get(library);
    }

    public static Class<? extends FunckyLibrary> getLibrary(final URI file) {
        return PRELUDE.entrySet().stream()
                .filter(library -> library.getValue().equals(file))
                .map(Map.Entry::getKey)
                .findFirst()
                .orElse(null);
    }

    public Linker(final FunckyEngine engine) throws IOException {
        this(engine, new File(engine.getFactory().getParameter(FunckyEngine.PARAMETER_BASE_DIR)).getCanonicalFile()
                .toURI());
    }

    private Linker(final FunckyEngine engine, final URI base) {
        this.engine = engine;
        this.base = base;
    }

    public FunckyExpression link(final FunckyExpression expression) {
        if (expression != null) {
            engine.getManager().setScript(new FunckyScript(expression)); // TODO decide which to keep
            final FunckyExpression exp = canonicalize(expression, new ArrayList<>());
            LOGGER.fine(exp.getType().toString()); // TODO get Type
            engine.getManager().setScript(new FunckyScript(exp)); // TODO decide which to keep
            return exp;
        }
        return null;
    }

    public FunckyScript link(final FunckyScript script, final boolean main) {
        final Class<? extends FunckyLibrary> library = getLibrary(script.getFile());
        if (library != null) {
            script.getDefinitions().addAll(loadLibrary(library).getDefinitions());
        }
        final FunckyScript scr = canonicalize(script);
        engine.getManager().setScript(scr); // TODO pick one to keep
        final Map<String, FunckyType> definitionTypes = validateDefinitions(scr);
        if (main) {
            validateMain(scr);
        }
        LOGGER.fine(script.getFile().toString());
        definitionTypes.entrySet().stream()
                .map(definitionType -> String.format(DEFINITION, definitionType.getKey(), definitionType.getValue()))
                .forEach(LOGGER::fine);
        engine.getManager().setScript(scr); // TODO pick one to keep
        return scr;
    }

    public InputStream getScript(final URI file) throws IOException {
        return Objects.requireNonNull((getLibrary(file) != null) ? Linker.class.getResource(
                String.format(PRELUDE_SCRIPT, file.getSchemeSpecificPart(),
                        engine.getFactory().getExtensions().getFirst())) : file.toURL()).openStream();
    }

    private FunckyLibrary loadLibrary(final Class<? extends FunckyLibrary> library) {
        try {
            return library.getDeclaredConstructor(FunckyEngine.class).newInstance(engine);
        } catch (final ReflectiveOperationException e) {
            throw new IllegalStateException(String.format(ERROR_LOADING_LIBRARY, getNamespace(library)), e);
        }
    }

    private FunckyScript canonicalize(final FunckyScript script) {
        final FunckyScript canonical = new FunckyScript(engine, script.getFile());
        canonical.getImports().addAll(canonicalizeImports(script.getImports()));
        canonical.getDefinitions().addAll(canonicalizeDefinitions(script.getDefinitions(), canonical.getImports()));
        return canonical;
    }

    private FunckyImport canonicalize(final FunckyImport inport) {
        return new FunckyImport(inport.file(), inport.line(), inport.prefix(),
                canonicalize(inport.file(), inport.namespace()));
    }

    private FunckyDefinition canonicalize(final FunckyDefinition definition, final List<FunckyImport> imports) {
        return new FunckyDefinition(definition.file(), definition.line(), definition.name(),
                canonicalize(definition.expression(), imports));
    }

    private FunckyExpression canonicalize(final FunckyExpression expression, final List<FunckyImport> imports) {
        return switch (expression) {
            case FunckyLiteral literal -> literal;
            case FunckyReference reference -> canonicalize(reference, imports);
            case FunckyApplication application -> canonicalize(application, imports);
        };
    }

    private FunckyReference canonicalize(final FunckyReference reference, final List<FunckyImport> imports) {
        return new FunckyReference(engine, reference.getFile(), reference.getLine(), reference.getColumn(), (reference.getNamespace() == null)
                ? ((reference.getPrefix() == null) ? reference.getFile() : resolve(reference, imports)): canonicalize(reference.getFile(), reference.getNamespace()), reference.getName());
    }

    private FunckyApplication canonicalize(final FunckyApplication application, final List<FunckyImport> imports) {
        return new FunckyApplication(canonicalize(application.getFunction(), imports),
                canonicalize(application.getArgument(), imports));
    }

    private URI canonicalize(final URI base, final URI namespace) { // TODO is this required? definitely base is not
        // required
        return (namespace.isAbsolute() ? namespace : (base.equals(STDIN) ? this.base : base).resolve(namespace))
                .normalize();
    }

    private List<FunckyImport> canonicalizeImports(final List<FunckyImport> imports) {
        final List<FunckyImport> canonical = new ArrayList<>();
        for (final FunckyImport inport : imports) {
            final Optional<FunckyImport> otherImport = imports.stream()
                    .filter(imp -> imp.line() < inport.line())
                    .filter(imp -> imp.prefix().equals(inport.prefix()))
                    .findFirst();
            if (otherImport.isPresent()) {
                throw new SneakyCompilationException(new PrefixAlreadyBoundException(inport, otherImport.get()));
            }
            canonical.add(canonicalize(inport));
        }
        return canonical;
    }

    private List<FunckyDefinition> canonicalizeDefinitions(final List<FunckyDefinition> definitions, final List<FunckyImport> imports) {
        final List<FunckyDefinition> canonical = new ArrayList<>();
        for (final FunckyDefinition definition : definitions) {
            final Optional<FunckyDefinition> otherDefinition = definitions.stream()
                    .filter(def -> def.line() < definition.line())
                    .filter(def -> def.name().equals(definition.name()))
                    .findFirst();
            if (otherDefinition.isPresent()) {
                throw new SneakyCompilationException(
                        new NameAlreadyDefinedException(definition, otherDefinition.get()));
            }
            canonical.add(canonicalize(definition, imports));
        }
        return canonical;
    }

    private URI resolve(final FunckyReference reference, final List<FunckyImport> imports) {
        final Optional<FunckyImport> inport = imports.stream()
                .filter(imp -> imp.prefix().equals(reference.getPrefix()))
                .findFirst();
        if (inport.isEmpty()) {
            throw new SneakyCompilationException(new UnboundPrefixException(reference));
        }
        return inport.get().namespace();
    }

    // TODO private
    public FunckyType getType(final FunckyExpression expression, Map<FunckyReference, FunckyTypeVariable> assumptions) {
        return switch (expression) {
            case FunckyLiteral literal -> getType(literal, assumptions);
            case FunckyReference reference -> getType(reference, assumptions);
            case FunckyApplication application -> getType(application, assumptions);
        };
    }

    private FunckyType getType(final FunckyLiteral literal,
            final Map<FunckyReference, FunckyTypeVariable> assumptions) {
        return literal.eval().getType();
    }

    private FunckyType getType(final FunckyReference reference,
            final Map<FunckyReference, FunckyTypeVariable> assumptions) {
        if (assumptions.containsKey(reference)) {
            return assumptions.get(reference);
        }
        final Map<FunckyReference, FunckyTypeVariable> newAssumptions = new HashMap<>(assumptions);
        newAssumptions.put(reference, new FunckyTypeVariable());
        return getType(reference.resolve().expression(), newAssumptions);
    }

    private FunckyType getType(final FunckyApplication application,
            final Map<FunckyReference, FunckyTypeVariable> assumptions) {
        final FunckyType functionType = getType(application.getFunction(), assumptions);
        final FunckyType argumentType = getType(application.getArgument(), assumptions);
        final FunckyFunctionType type = (FunckyFunctionType) functionType
                .unify(FUNCTION(argumentType, new FunckyTypeVariable()));
        if (type != null) {
            return ((FunckyType) type.getRange().eval(engine.getContext()));
        } else {
            throw new SneakyCompilationException(new IllegalApplicationException(application, functionType,
                    argumentType));
        }
    }

    // TODO remove
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
        if (main.get().expression().getType().unify(MAIN_TYPE) == null) {
            throw new SneakyCompilationException(new InvalidMainException(main.get()));
        }
    }
}
