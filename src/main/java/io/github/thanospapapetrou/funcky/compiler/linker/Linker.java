package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.logging.Logger;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyFactory;
import io.github.thanospapapetrou.funcky.compiler.ast.Expression;
import io.github.thanospapapetrou.funcky.compiler.ast.Import;
import io.github.thanospapapetrou.funcky.compiler.ast.Script;
import io.github.thanospapapetrou.funcky.compiler.exceptions.IllegalApplicationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.InvalidMainException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.PrefixAlreadyBoundException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnboundPrefixException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UndefinedMainException;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
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

    public FunckyExpression link(final Expression expression) {
        if (expression != null) {
            final FunckyExpression typed = link(new Script(expression), false).getDefinitions().getFirst().expression();
            LOGGER.fine(typed.getType().toString());
            return typed; // TODO return script
        }
        return null;
    }

    public FunckyScript link(final Script script, final boolean main) {
        //        final FunckyScript typed = new FunckyScript(engine, script.file());
        //        final Class<? extends FunckyLibrary> library = getLibrary(script.file());
        //        if (library != null) {
        //            typed.getDefinitions().addAll(loadLibrary(library).getDefinitions());
        //        }
        //
        //        typed.definitions.addAll(script.definitions().stream()
        //                .map(this::link)
        //                .toList());
        //        final FunckyScript typed = inferTypes(canonicalize(script), main);
        //        LOGGER.fine(typed.toString());
        //        typed.getDefinitions().stream()
        //                .map(definition -> String.format(DEFINITION, definition.name(), definition.expression().getType()))
        //                .forEach(LOGGER::fine);
        //        return typed;
        return null;
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

    private Script canonicalize(final Script script) {
        //        // TODO canonical script does not need imports
        //        final Script canonical = new Script(script.file());
        //        canonical.imports().addAll(canonicalizeImports(script.imports()));
        //        canonical.definitions().addAll(canonicalizeDefinitions(script.definitions(), canonical.imports()));
        //        engine.getManager().setScript(canonical); // TODO is this needed?
        //        return canonical;
        return null;
    }

    private Import canonicalize(final Import inport) {
        return new Import(inport.file(), inport.line(), inport.prefix(),
                canonicalize(inport.file(), inport.namespace()));
    }

    //    private FunckyDefinition canonicalize(final FunckyDefinition definition, final List<FunckyImport> imports) {
    //        return new FunckyDefinition(definition.file(), definition.line(), definition.name(),
    //                canonicalize(definition.expression(), imports));
    //    }

    //    private FunckyExpression canonicalize(final FunckyExpression expression, final List<FunckyImport> imports) {
    //        return switch (expression) {
    //            case FunckyLiteral literal -> literal;
    //            case FunckyReference reference -> canonicalize(reference, imports);
    //            case FunckyApplication application -> canonicalize(application, imports);
    //        };
    //    }

    //    private FunckyReference canonicalize(final FunckyReference reference, final List<FunckyImport> imports) {
    //        return new FunckyReference(engine, reference.getFile(), reference.getLine(), reference.getColumn(), (reference.getNamespace() == null)
    //                ? ((reference.getPrefix() == null) ? reference.getFile() : resolve(reference, imports)) :
    //                canonicalize(reference.getFile(), reference.getNamespace()), reference.getPrefix(),
    //                reference.getName(), reference.getType());
    //    }
    //
    //    private FunckyApplication canonicalize(final FunckyApplication application, final List<FunckyImport> imports) {
    //        return new FunckyApplication(canonicalize(application.getFunction(), imports),
    //                canonicalize(application.getArgument(), imports));
    //    }

    private URI canonicalize(final URI base, final URI namespace) { // TODO is this required? definitely base is not
        // required
        return (namespace.isAbsolute() ? namespace : (base.equals(STDIN) ? this.base : base).resolve(namespace))
                .normalize();
    }

    private List<Import> canonicalizeImports(final List<Import> imports) {
        final List<Import> canonical = new ArrayList<>();
        for (final Import inport : imports) {
            final Optional<Import> otherImport = imports.stream()
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

    //    private List<Definition> canonicalizeDefinitions(final List<FunckyDefinition> definitions, final List<FunckyImport> imports) {
    //        final List<FunckyDefinition> canonical = new ArrayList<>();
    //        for (final FunckyDefinition definition : definitions) {
    //            final Optional<FunckyDefinition> otherDefinition = definitions.stream()
    //                    .filter(def -> def.line() < definition.line())
    //                    .filter(def -> def.name().equals(definition.name()))
    //                    .findFirst();
    //            if (otherDefinition.isPresent()) {
    //                throw new SneakyCompilationException(
    //                        new NameAlreadyDefinedException(definition, otherDefinition.get()));
    //            }
    //            canonical.add(canonicalize(definition, imports));
    //        }
    //        return canonical;
    //        return null;
    //    }

    private URI resolve(final FunckyReference reference, final List<Import> imports) {
        final Optional<Import> inport = imports.stream()
                .filter(imp -> imp.prefix().equals(reference.getPrefix()))
                .findFirst();
        if (inport.isEmpty()) {
            throw new SneakyCompilationException(new UnboundPrefixException(reference));
        }
        return inport.get().namespace();
    }

    private FunckyScript inferTypes(final FunckyScript script, final boolean main) {
        //        final FunckyScript typed = new FunckyScript(engine, script.getFile());
        //        final Class<? extends FunckyLibrary> library = getLibrary(script.getFile());
        //        if (library != null) {
        //            typed.getDefinitions().addAll(loadLibrary(library).getDefinitions()); // TODO
        //        }
        //
        //        typed.getImports().addAll(script.getImports());
        //        typed.getDefinitions().addAll(script.getDefinitions().stream()
        //                .map(this::inferTypes)
        //                .toList());
        //        if (main) {
        //            validateMain(typed);
        //        }
        //        engine.getManager().setScript(typed);
        //        return typed;
        return null;
    }

    private FunckyDefinition inferTypes(final FunckyDefinition definition) {
        return new FunckyDefinition(definition.file(), definition.line(), definition.name(),
                inferTypes(definition.expression(), Map.of()));
    }

    private FunckyExpression inferTypes(final FunckyExpression expression,
            Map<FunckyReference, FunckyTypeVariable> assumptions) {
        return switch (expression) {
            case FunckyLiteral literal -> inferTypes(literal, assumptions);
            case FunckyReference reference -> inferTypes(reference, assumptions);
            case FunckyApplication application -> inferTypes(application, assumptions);
        };
    }

    private FunckyLiteral inferTypes(final FunckyLiteral literal,
            final Map<FunckyReference, FunckyTypeVariable> assumptions) {
        return new FunckyLiteral(engine, literal.getFile(), literal.getLine(), literal.getColumn(),
                inferTypes(literal.eval()));
    }

    private FunckyReference inferTypes(final FunckyReference reference,
            final Map<FunckyReference, FunckyTypeVariable> assumptions) {
        final FunckyType type = reference.resolve().expression().getType();
        if (type != null) {
            return new FunckyReference(engine, reference.getFile(), reference.getLine(), reference.getColumn(),
                    reference.getNamespace(), reference.getPrefix(), reference.getName(), type);
        }
        if (assumptions.containsKey(reference)) {
            return new FunckyReference(engine, reference.getFile(), reference.getLine(), reference.getColumn(),
                    reference.getNamespace(), reference.getPrefix(), reference.getName(), assumptions.get(reference));
        }
        final Map<FunckyReference, FunckyTypeVariable> newAssumptions = new HashMap<>(assumptions);
        newAssumptions.put(reference, new FunckyTypeVariable());
        return new FunckyReference(engine, reference.getFile(), reference.getLine(), reference.getColumn(),
                reference.getNamespace(), reference.getPrefix(), reference.getName(),
                inferTypes(reference.resolve().expression(), newAssumptions).getType());
    }

    private FunckyApplication inferTypes(final FunckyApplication application,
            final Map<FunckyReference, FunckyTypeVariable> assumptions) {
        final FunckyExpression function = inferTypes(application.getFunction(), assumptions);
        final FunckyExpression argument = inferTypes(application.getArgument(), assumptions);
        final FunckyFunctionType type = (FunckyFunctionType) function.getType()
                .unify(FUNCTION(argument.getType(), new FunckyTypeVariable()));
        if (type != null) {
            return new FunckyApplication(function, argument, (FunckyType) type.getRange().eval(engine.getContext()));
        } else {
            throw new SneakyCompilationException(new IllegalApplicationException(application, function.getType(),
                    argument.getType()));
        }
    }

    private FunckyValue inferTypes(final FunckyValue value) {
        return switch (value) {
            case FunckyType type -> type;
            case FunckyNumber number -> number;
            case FunckyBoolean bool -> bool;
            case FunckyCharacter character -> character;
            case FunckyFunction function -> function;
            case FunckyList list -> null;
            case FunckyRecord record -> null;
        };
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
