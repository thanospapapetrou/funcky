package io.github.thanospapapetrou.funcky.compiler.linker;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.function.Function;
import java.util.logging.Logger;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyFactory;
import io.github.thanospapapetrou.funcky.compiler.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.InvalidListLiteralException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.InvalidMainException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.NameAlreadyDefinedException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.PrefixAlreadyBoundException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.UnboundPrefixException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.UndefinedMainException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.UndefinedNameException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.nativ.NativeClassNotFoundException;
import io.github.thanospapapetrou.funcky.compiler.linker.exceptions.nativ.NativeFieldNotFoundException;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;
import io.github.thanospapapetrou.funcky.runtime.prelude.HigherOrderFunction;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER;

public class Linker {
    public static final Function<FunckyEngine, FunckyFunctionType> MAIN_TYPE = FUNCTION(LIST(STRING), NUMBER);
    public static final String SCHEME_FUNCKY =
            FunckyFactory.getParameters(FunckyEngine.LANGUAGE).getFirst().toLowerCase(Locale.ROOT);
    public static final String SCHEME_JAVA = "java";
    public static final URI STDIN;

    private static final String DEFINITION = "%1$s\t%2$s";
    private static final String ERROR_CANONICALIZING_NAMESPACE = "Error canonicalizing namespace %1$s";
    private static final Logger LOGGER = Logger.getLogger(Linker.class.getName());
    private static final String MESSAGE_END = "Linked `%1$s` in %2$d ms";
    private static final String MESSAGE_START = "Linking `%1$s`...";
    private static final String PRELUDE_SCRIPT = "/prelude/%1$s.%2$s";
    private static final String USER_DIR = "user.dir";

    private final FunckyEngine engine;
    private final Clock clock;

    static {
        try {
            STDIN = new URI(SCHEME_FUNCKY, "stdin", null);
        } catch (final URISyntaxException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static URL getScript(final URI file) throws IOException {
        if (file.getScheme().equals(SCHEME_FUNCKY)) {
            final Class<? extends FunckyLibrary> library = FunckyLibrary.getLibrary(file);
            if (library == null) {
                // TOOD
                throw new IOException(String.format("No prelude library found for %1$s", file));
            }
            return Linker.class.getResource(String.format(PRELUDE_SCRIPT, file.getSchemeSpecificPart(),
                    FunckyFactory.getParameters(FunckyEngine.PARAMETER_EXTENSIONS).getFirst()));
        }
        return file.toURL();
    }

    public Linker(final FunckyEngine engine, final Clock clock) {
        this.engine = engine;
        this.clock = clock;
    }

    // TODO is this required public here?
    public URI canonicalize(final URI base, final URI namespace) {
        try {
            return namespace.isAbsolute() ? namespace : (base.equals(STDIN)
                    ? new File(System.getProperty(USER_DIR)).getCanonicalFile().toURI() : base).resolve(namespace);
        } catch (final IOException e) {
            throw new IllegalStateException(String.format(ERROR_CANONICALIZING_NAMESPACE, namespace), e);
        }
    }

    public FunckyExpression link(final FunckyExpression expression) {
        LOGGER.fine(String.format(MESSAGE_START, Linker.STDIN));
        final Instant start = clock.instant();
        if (expression == null) {
            return null;
        }
        engine.getContext().setScript(STDIN);
        final FunckyExpression typed = checkTypes(canonicalize(expression));
        LOGGER.fine(typed.getType().toString());
        LOGGER.fine(String.format(MESSAGE_END, Linker.STDIN, Duration.between(start, clock.instant()).toMillis()));
        LOGGER.fine("");
        return typed;
    }

    public FunckyScript link(final FunckyScript script, final boolean main) {
        LOGGER.fine(String.format(MESSAGE_START, script.getFile()));
        final Instant start = clock.instant();
        final FunckyScript checked = checkTypes(canonicalize(script), main);
        checked.getDefinitions().stream()
                .map(definition -> String.format(DEFINITION, definition.name(), definition.expression().getType()))
                .forEach(LOGGER::fine);
        LOGGER.fine(String.format(MESSAGE_END, script.getFile(), Duration.between(start, clock.instant()).toMillis()));
        LOGGER.fine("");
        return checked;
    }

    private FunckyScript canonicalize(final FunckyScript script) {
        final FunckyScript canonical = new FunckyScript(engine, script.getFile());
        engine.getContext().setScript(canonical.getFile());
        script.getImports().stream()
                .map(this::canonicalize)
                .forEach(canonical.getImports()::add);
        script.getDefinitions().stream()
                .map(this::canonicalize)
                .forEach(canonical.getDefinitions()::add);
        return canonical;
    }

    private FunckyImport canonicalize(final FunckyImport inport) {
        final FunckyImport other = engine.getContext().getImport(inport.file(), inport.prefix());
        if (other != null) {
            throw new SneakyCompilationException(new PrefixAlreadyBoundException(inport, other));
        }
        final FunckyImport canonical = new FunckyImport(inport.file(), inport.line(), inport.prefix(),
                canonicalize(inport.file(), inport.namespace()));
        engine.getContext().setImport(canonical);
        return canonical;
    }

    private FunckyDefinition canonicalize(final FunckyDefinition definition) {
        final FunckyDefinition other = engine.getContext().getDefinition(definition.file(), definition.name());
        if (other != null) {
            throw new SneakyCompilationException(new NameAlreadyDefinedException(definition, other));
        }
        final FunckyDefinition canonical = new FunckyDefinition(definition.file(), definition.line(),
                definition.name(), ((definition.expression() instanceof FunckyReference reference)
                && (reference.getNamespace() != null) && reference.getNamespace().getScheme().equals(SCHEME_JAVA))
                ? loadNative(definition, reference) : canonicalize(definition.expression()));
        engine.getContext().setDefinition(canonical);
        return canonical;
    }

    private FunckyExpression canonicalize(final FunckyExpression expression) {
        return switch (expression) {
            case FunckyLiteral literal -> canonicalize(literal);
            case FunckyReference reference -> canonicalize(reference);
            case FunckyApplication application -> canonicalize(application);
        };
    }

    private FunckyLiteral canonicalize(final FunckyLiteral literal) {
        return new FunckyLiteral(engine, literal.getFile(), literal.getLine(), literal.getColumn(),
                canonicalize(literal.eval(engine.getContext())));
    }

    private FunckyReference canonicalize(final FunckyReference reference) {
        return new FunckyReference(engine, reference.getFile(), reference.getLine(), reference.getColumn(),
                reference.getNamespace(), reference.getPrefix(), (reference.getNamespace() == null)
                ? ((reference.getPrefix() == null) ? reference.getFile() : resolvePrefix(reference))
                : engine.getLinker().canonicalize(reference.getFile(), reference.getNamespace()), reference.getName());
    }

    private FunckyApplication canonicalize(final FunckyApplication application) {
        return new FunckyApplication(canonicalize(application.getFunction()), canonicalize(application.getArgument()));
    }

    private FunckyValue canonicalize(final FunckyValue value) {
        return switch (value) {
            case FunckyList list -> canonicalize(list);
            case FunckyRecord record -> canonicalize(record);
            default -> value;
        };
    }

    private FunckyList canonicalize(final FunckyList list) {
        return new FunckyList(engine, list.getType(), (list.getHead() == null) ? null : canonicalize(list.getHead()),
                (list.getTail() == null) ? null : canonicalize(list.getTail()));
    }

    private FunckyRecord canonicalize(final FunckyRecord record) {
        return new FunckyRecord(engine, record.getType(), record.getComponents().stream()
                .map(this::canonicalize)
                .toList());
    }

    private FunckyScript checkTypes(final FunckyScript script, final boolean main) {
        final FunckyScript checked = new FunckyScript(engine, script.getFile());
        checked.getImports().addAll(script.getImports());
        script.getDefinitions().stream()
                .map(this::checkTypes)
                .forEach(checked.getDefinitions()::add);
        if (main) {
            final Optional<FunckyDefinition> mane = checked.getDefinitions().stream()
                    .filter(def -> def.name().equals(FunckyScript.MAIN))
                    .findAny();
            if (mane.isEmpty()) {
                throw new SneakyCompilationException(new UndefinedMainException(checked));
            }
            if (mane.get().expression().getType().unify(MAIN_TYPE.apply(engine)) == null) {
                throw new SneakyCompilationException(new InvalidMainException(mane.get()));
            }
        }
        return checked;
    }

    private FunckyDefinition checkTypes(final FunckyDefinition definition) {
        final FunckyDefinition checked = new FunckyDefinition(definition.file(), definition.line(), definition.name(),
                checkTypes(definition.expression()));
        if (engine.getContext().getType(checked.file(), checked.name()) == null) {
            engine.getContext().setType(checked.file(), checked.name(), new FunckyTypeVariable(engine));
            engine.getContext().setType(checked.file(), checked.name(), checked.expression().getType());
        }
        return checked;
    }

    private FunckyExpression checkTypes(final FunckyExpression expression) {
        return switch (expression) {
            case FunckyLiteral literal -> checkTypes(literal);
            case FunckyReference reference -> checkTypes(reference);
            case FunckyApplication application -> checkTypes(application);
        };
    }

    private FunckyLiteral checkTypes(final FunckyLiteral literal) {
        return new FunckyLiteral(engine, literal.getFile(), literal.getLine(), literal.getColumn(),
                checkTypes(literal.eval(engine.getContext())));
    }

    private FunckyReference checkTypes(final FunckyReference reference) {
        if (engine.getContext().getScript(reference.getCanonical()) == null) {
            try {
                engine.compile(reference.getCanonical());
            } catch (final FunckyCompilationException e) {
                throw new SneakyCompilationException(e);
            }
        }
        if (engine.getContext().getDefinition(reference.getCanonical(), reference.getName()) == null) {
            throw new SneakyCompilationException(new UndefinedNameException(reference));
        }
        return reference;
    }

    private FunckyApplication checkTypes(final FunckyApplication application) {
        return new FunckyApplication(checkTypes(application.getFunction()), checkTypes(application.getArgument()));
    }

    private FunckyValue checkTypes(final FunckyValue value) {
        return (value instanceof FunckyList list) ? checkTypes(list) : value;
    }

    private FunckyList checkTypes(final FunckyList list) {
        if ((list.getType().getElement() instanceof FunckyLiteral literal)
                && (literal.eval(engine.getContext()) instanceof FunckyTypeVariable)) {
            final FunckyExpression head = (list.getHead() == null) ? null : checkTypes(list.getHead());
            final FunckyExpression tail = (list.getTail() == null) ? null : checkTypes(list.getTail());
            final FunckyType headType = (head == null) ? new FunckyTypeVariable(engine) : head.getType();
            final FunckyListType tailType = (tail == null) ? LIST(FunckyTypeVariable::new).apply(engine)
                    : (FunckyListType) tail.getType();
            final FunckyListType type = (FunckyListType) LIST(engine -> headType).apply(engine)
                    .unify(tailType);
            if (type == null) {
                throw new SneakyCompilationException(new InvalidListLiteralException(head, headType, tail, tailType));
            }
            return imposeType(type, head, tail);
        }
        return list;
    }

    private FunckyExpression loadNative(final FunckyDefinition definition, final FunckyReference reference) {
        try {
            final Class<?> clazz = Class.forName(reference.getNamespace().getSchemeSpecificPart());
            final Field field = clazz.getDeclaredField(reference.getName());
            if (FunckyValue.class.isAssignableFrom(field.getType())) {
                return new FunckyLiteral(engine, reference.getFile(), reference.getLine(), reference.getColumn(),
                        loadNative(definition, (FunckyValue) field.get(
                                clazz.getDeclaredConstructor(FunckyEngine.class).newInstance(engine))));
            } else {
                throw new RuntimeException("Not a FunckyValue"); // TODO
            }
        } catch (final ClassNotFoundException e) {
            throw new SneakyCompilationException(new NativeClassNotFoundException(reference));
        } catch (final NoSuchFieldException e) {
            throw new SneakyCompilationException(new NativeFieldNotFoundException(reference));
        } catch (final NoSuchMethodException e) {
            throw new RuntimeException("Constructor not found", e); // TODO
        } catch (final ReflectiveOperationException e) {
            throw new RuntimeException("Error instantiating class", e); // TODO
        }
    }

    private FunckyValue loadNative(final FunckyDefinition definition, final FunckyValue value) {
        return (value instanceof HigherOrderFunction function)
                ? new HigherOrderFunction(engine, function.getType(), function.getOrder(),
                new FunckyReference(engine, definition.file(), definition.name())) {
            @Override
            public FunckyValue apply(final List<FunckyExpression> arguments, final ScriptContext context) {
                return function.apply(arguments, context);
            }
        } : value;
    }

    private URI resolvePrefix(final FunckyReference reference) {
        final FunckyImport inport = engine.getContext().getImport(reference.getFile(), reference.getPrefix());
        if (inport == null) {
            throw new SneakyCompilationException(new UnboundPrefixException(reference));
        }
        return inport.namespace();
    }

    private FunckyList imposeType(final FunckyListType type, final FunckyExpression head, final FunckyExpression tail) {
        return new FunckyList(engine, type, head, (tail instanceof FunckyLiteral literal)
                && (literal.eval(engine.getContext()) instanceof FunckyList list)
                ? new FunckyLiteral(engine, imposeType(type, list.getHead(), list.getTail())) : tail);
    }
}
