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
import java.util.List;
import java.util.Locale;
import java.util.Objects;
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
    public static final String PRELUDE_SCHEME =
            FunckyFactory.getParameters(FunckyEngine.LANGUAGE).getFirst().toLowerCase(Locale.ROOT);
    public static final URI STDIN;

    private static final String DEFINITION = "  %1$s%n    %2$s";
    private static final String ERROR_CANONICALIZING_NAMESPACE = "Error canonicalizing namespace %1$s";
    private static final Logger LOGGER = Logger.getLogger(Linker.class.getName());
    private static final String PRELUDE_SCRIPT = "/prelude/%1$s.%2$s";
    private static final String USER_DIR = "user.dir";

    private final FunckyEngine engine;

    static {
        try {
            STDIN = new URI(PRELUDE_SCHEME, "stdin", null);
        } catch (final URISyntaxException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public Linker(final FunckyEngine engine) {
        this.engine = engine;
    }

    public URI canonicalize(final URI base, final URI namespace) {
        try {
            return namespace.isAbsolute() ? namespace
                    : (base.equals(STDIN) ? new File(System.getProperty(USER_DIR)).getCanonicalFile().toURI()
                            : base).resolve(namespace);
        } catch (final IOException e) {
            throw new IllegalStateException(String.format(ERROR_CANONICALIZING_NAMESPACE, namespace), e);
        }
    }

    public FunckyExpression link(final FunckyExpression expression) {
        if (expression == null) {
            return null;
        }
        engine.getContext().setScript(STDIN);
        final FunckyExpression typed = checkTypes(canonicalize(expression));
        LOGGER.fine(typed.getType().toString());
        LOGGER.fine(engine.getContext().toString());
        return typed;
    }

    public FunckyScript link(final FunckyScript script, final boolean main) {
        final FunckyScript checked = checkTypes(canonicalize(script), main);
        LOGGER.fine(checked.getFile().toString());
        checked.getDefinitions().stream()
                .map(definition -> String.format(DEFINITION, definition.name(), definition.expression().getType()))
                .forEach(LOGGER::fine);
        LOGGER.fine(engine.getContext().toString());
        return checked;
    }

    public InputStream getScript(final URI file) throws IOException {
        return Objects.requireNonNull((getLibrary(file) != null) ? Linker.class.getResource(
                String.format(PRELUDE_SCRIPT, file.getSchemeSpecificPart(),
                        engine.getFactory().getExtensions().getFirst())) : file.toURL()).openStream();
    }

    @SuppressWarnings("unchecked")
    private Class<? extends FunckyLibrary> getLibrary(final URI file) {
        return (Class<? extends FunckyLibrary>) Arrays.stream(FunckyLibrary.class.getPermittedSubclasses())
                .filter(library -> FunckyLibrary.getNamespace((Class<? extends FunckyLibrary>) library).equals(file))
                .findFirst()
                .orElse(null);
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
        FunckyDefinition canonical = null; // TODO improve
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
                        canonical = new FunckyDefinition(definition.file(),
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
                        canonical = new FunckyDefinition(definition.file(),
                                definition.line(), definition.name(), new FunckyLiteral(engine,
                                reference.getFile(), reference.getLine(), reference.getColumn(),
                                (FunckyValue) field.get(constructor.newInstance(engine))));
                    }
                } else {
                    throw new RuntimeException("Not a FunckyValue");
                }
            } catch (final ClassNotFoundException | NoSuchFieldException | InstantiationException |
                    IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
        } else {
            canonical = new FunckyDefinition(definition.file(), definition.line(), definition.name(),
                    canonicalize(definition.expression())); // TODO
        }
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
        URI canonical = null;
        if (reference.getNamespace() == null) {
            if (reference.getPrefix() == null) {
                canonical = reference.getFile();
            } else {
                final FunckyImport inport = engine.getContext().getImport(reference.getFile(), reference.getPrefix());
                if (inport == null) {
                    throw new SneakyCompilationException(new UnboundPrefixException(reference));
                }
                canonical = inport.namespace();
            }
        } else {
            canonical = engine.getLinker().canonicalize(reference.getFile(), reference.getNamespace());
        } // TODO improve

        return new FunckyReference(engine, reference.getFile(), reference.getLine(), reference.getColumn(),
                reference.getNamespace(), reference.getPrefix(), canonical, reference.getName());
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
            final FunckyListType tailType = (tail == null) ? FunckyListType.LIST(FunckyTypeVariable::new).apply(engine)
                    : (FunckyListType) tail.getType();
            final FunckyListType type = (FunckyListType) new FunckyListType(engine, new FunckyLiteral(engine, headType))
                    .unify(tailType);
            if (type == null) {
                throw new SneakyCompilationException(new InvalidListLiteralException(head, headType, tail, tailType));
            }
            return imposeType(type, head, tail);
        }
        return list;
    }

    private FunckyList imposeType(final FunckyListType type, final FunckyExpression head, final FunckyExpression tail) {
        return new FunckyList(engine, type, head, (tail instanceof FunckyLiteral literal)
                && (literal.eval(engine.getContext()) instanceof FunckyList list)
                ? new FunckyLiteral(engine, imposeType(type, list.getHead(), list.getTail())) : tail);
    }
}
