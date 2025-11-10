package io.github.thanospapapetrou.funcky.compiler.transpiler;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.exceptions.FunckyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.TranspilationException;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;

public class Transpiler {
    // TODO cleanup
    // TODO types, values, to string, equals, hashcode in transpiled code
    public static final String PREFIX_FUNCKY = "$";

    private static final String DELIMITER_EXTENSION = ".";
    private static final String DELIMITER_PACKAGE = ".";
    private static final String ERROR_LOADING = "Error loading %1$s!%2$s$%3$s";
    private static final String EXTENSION_CLASS = "class";
    private static final String EXTENSION_JAR = "jar";
    private static final String EXTENSION_JAVA = "java";
    private static final String JAVA_APPLICATION = "new %1$s(%2$s, %3$s)";
    private static final String JAVA_DEFINITION = "    public final %1$s %2$s%3$s = %4$s.eval(engine.getContext());%n";
    private static final String JAVA_DEPENDENCY = "    private final %1$s %2$s = new %1$s(engine);%n";
    private static final String JAVA_LITERAL = "new %1$s(engine, %2$s.create(\"%3$s\"), %4$d, %5$d, %6$s)";
    private static final String JAVA_REFERENCE = "new %1$s(engine, %2$s.%3$s%4$s)";
    private static final String JAVA_SCRIPT = "package %1$s;%n%npublic class %2$s extends %3$s {%n"
            + "    public %2$s(final %4$s engine) {%n        super(engine%5$s);%n    }%n%6$s%7$s}";
    private static final String JAVA_URI = ", %1$s.create(\"%2$s\")";
    private static final String MANIFEST_VERSION = "1.0";
    private static final String OPTION_OUTPUT = "-d";
    private static final String PATTERN_NON_WORD = "\\W+";
    private static final String PREFIX_JAVA = "_";

    private final FunckyEngine engine;
    private final JavaCompiler compiler;
    private final File tmp;
    private final File output;

    public Transpiler(final FunckyEngine engine) throws IOException {
        this(engine, ToolProvider.getSystemJavaCompiler(),
                new File(engine.getFactory().getParameter(FunckyEngine.PARAMETER_TMP_DIR)).getCanonicalFile(),
                new File(engine.getFactory().getParameter(FunckyEngine.PARAMETER_OUTPUT_DIR)).getCanonicalFile());
    }

    private Transpiler(final FunckyEngine engine, final JavaCompiler compiler, final File tmp, final File output) {
        this.engine = engine;
        this.compiler = compiler;
        this.tmp = tmp;
        this.output = output;
    }

    public String getClass(final URI script) {
        return PREFIX_FUNCKY + String.join(PREFIX_FUNCKY, script.toString().split(PATTERN_NON_WORD));
    }

    public FunckyExpression transpile(final FunckyExpression expression) {
        if (expression == null) {
            return null;
        }
        final File java = generateJava(expression);
//        compile(java); TODO
//        return load(packadze(expression, java), java, expression);
        return null;
    }

    public FunckyScript transpile(final FunckyScript script) {
        try {
            final Set<File> java = new HashSet<>();
            // TODO
//            for (final FunckyScript dependency : script.getDependencies()) {
//                java.add(generateJava(dependency));
//            }
            compile(java);
            return load(packadze(java), getClass(script.getFile()));
        } catch (final IOException e) {
            throw new SneakyCompilationException(new FunckyCompilationException(e));
        } catch (final ReflectiveOperationException e) {
            throw new RuntimeException(e); // TODO
        }
    }

    private File generateJava(final FunckyExpression expression) {
        return null;
    }

    private File generateJava(final FunckyScript script) throws IOException {
        final List<String> components = getComponents(script.getFile());
        final File java =
                new File(tmp,
                        String.join(File.separator, components) + DELIMITER_EXTENSION + EXTENSION_JAVA);
        java.getParentFile().mkdirs();
        java.createNewFile();
        try (final FileWriter writer = new FileWriter(java, StandardCharsets.UTF_8)) {
            writer.write(toJava(script));
            writer.flush();
        }
        return java;
    }

    private List<String> getComponents(final URI script) {
        return Arrays.stream(removeExtension(script).split(PATTERN_NON_WORD))
                .filter(((Predicate<String>) String::isEmpty).negate())
                .map(component -> Character.isJavaIdentifierStart(component.charAt(0)) ? component
                        : PREFIX_JAVA + component)
                .toList();
    }

    private String removeExtension(final URI script) {
        for (final String extension : engine.getFactory().getExtensions()) {
            if (script.toString().endsWith(DELIMITER_EXTENSION + extension)) {
                return script.toString().substring(0,
                        script.toString().length() - (DELIMITER_EXTENSION + extension).length());
            }
        }
        return script.toString();
    }

    private String toJava(final FunckyScript script) {
        final List<String> components = getComponents(script.getFile());
        final String packadze = String.join(DELIMITER_PACKAGE, components.subList(0, components.size() - 1));
        final String clazz = components.getLast();
        final Class<? extends FunckyLibrary> library = Linker.getLibrary(script.getFile());
        final String parent = ((library == null) ? FunckyScript.class : library).getName();
        return String.format(JAVA_SCRIPT, packadze, clazz, parent, FunckyEngine.class.getName(),
                (library == null) ? String.format(JAVA_URI, URI.class.getName(),
                        EscapeHelper.escape(script.getFile().toString())) : "",
                // TODO
                null,
//                script.getDependencies().stream()
//                        .map(FunckyScript::getFile)
//                        .map(this::toJava)
//                        .collect(Collectors.joining()),
                script.getDefinitions().stream()
                        .filter(definition -> definition.line() != -1)
                        .map(this::toJava)
                        .collect(Collectors.joining()));
    }

    private String toJava(final URI dependency) {
        return String.format(JAVA_DEPENDENCY, String.join(DELIMITER_PACKAGE, getComponents(dependency)),
                String.join(PREFIX_JAVA, getComponents(dependency)));
    }

    private String toJava(final FunckyDefinition definition) {
        return String.format(JAVA_DEFINITION, FunckyValue.class.getName(), PREFIX_FUNCKY, definition.name(),
                toJava(definition.expression()));
    }

    private String toJava(final FunckyExpression expression) {
        return switch (expression) {
            case FunckyLiteral literal -> toJava(literal);
            case FunckyReference reference -> toJava(reference);
            case FunckyApplication application -> toJava(application);
        };
    }

    private String toJava(final FunckyLiteral literal) {
        return String.format(JAVA_LITERAL, FunckyLiteral.class.getName(), URI.class.getName(), literal.getFile(),
                literal.getLine(), literal.getColumn(), toJava(literal.eval()));
    }

    private String toJava(final FunckyReference reference) {
        return String.format(JAVA_REFERENCE, FunckyLiteral.class.getName(), String.join(PREFIX_JAVA, // TODO getclass
                        getComponents(reference.getNamespace())),
                PREFIX_FUNCKY, reference.getName());
    }

    private String toJava(final FunckyApplication application) {
        return String.format(JAVA_APPLICATION, FunckyApplication.class.getName(), toJava(application.getFunction()),
                toJava(application.getArgument()));
    }

    private String toJava(final FunckyValue value) {
        return switch (value) {
            case FunckyType type -> toJava(type);
            case FunckyNumber number -> toJava(number);
            case FunckyBoolean bool -> toJava(bool);
            case FunckyCharacter character -> toJava(character);
            case FunckyFunction function -> toJava(function);
            case FunckyList list -> toJava(list);
            case FunckyRecord record -> toJava(record);
        };
    }

    private String toJava(final FunckyType type) {
        return switch (type) {
            case FunckySimpleType simple -> toJava(simple);
            case FunckyFunctionType function -> toJava(function);
            case FunckyListType list -> toJava(list);
            case FunckyRecordType record -> toJava(record);
            case FunckyTypeVariable variable -> toJava(variable);
        };
    }

    private String toJava(final FunckySimpleType simple) {
        return null;
    }

    private String toJava(final FunckyNumber number) {
        return null;
    }

    private String toJava(final FunckyBoolean bool) {
        return null;
    }

    private String toJava(final FunckyCharacter character) {
        return null;
    }

    private String toJava(final FunckyFunction function) {
        return null;
    }

    private String toJava(final FunckyList list) {
        return null;
    }

    private String toJava(final FunckyRecord record) {
        return null;
    }

    private void compile(final Set<File> java) throws IOException {
        final DiagnosticCollector<JavaFileObject> collector = new DiagnosticCollector<>();
        try (final StandardJavaFileManager manager = compiler.getStandardFileManager(collector, Locale.ROOT,
                StandardCharsets.UTF_8)) {
            final boolean compilation = compiler.getTask(null, manager, collector, List.of(OPTION_OUTPUT,
                            tmp.getPath()), null,
                    manager.getJavaFileObjectsFromFiles(java)).call();
            // TODO java.delete();
            if (!compilation) {
                throw new SneakyCompilationException(new TranspilationException(collector.getDiagnostics()));
            }
        }
    }

    private File packadze(final Set<File> java) throws IOException {
        final File jar = new File(output, "TODO"
                + DELIMITER_EXTENSION + EXTENSION_JAR);
        try (final JarOutputStream output = new JarOutputStream(new FileOutputStream(jar), getManifest())) {
            for (final File clazz : Objects.requireNonNull(tmp
                    .listFiles((dir, name) -> name.startsWith("TODO")
                            && name.endsWith(DELIMITER_EXTENSION + EXTENSION_CLASS)))) {
                output.putNextEntry(new ZipEntry(clazz.getName()));
                Files.copy(clazz.toPath(), output);
                output.closeEntry();
                clazz.delete();
            }
            try (final JarInputStream input = new JarInputStream(
            engine.getClass().getProtectionDomain().getCodeSource().getLocation().openStream())) {
                JarEntry entry;
                while ((entry = input.getNextJarEntry()) != null) {
                    output.putNextEntry(new JarEntry(entry.getName()));
                    output.write(input.readAllBytes());
                    output.closeEntry();
                }
            }
        }
        return jar;
    }

    private <T> T load(final File jar, final String clazz) throws IOException,
            ReflectiveOperationException {
        try (final URLClassLoader loader = new URLClassLoader(new URL[]{jar.toURI().toURL()})) {
            final Constructor<?> constructor = loader.loadClass(clazz)
                    .getDeclaredConstructor(FunckyEngine.class);
            constructor.setAccessible(true); // TODO is this required?
            return (T) constructor.newInstance(engine);
        }
    }

    private Manifest getManifest() {
        final Manifest manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, MANIFEST_VERSION);
        manifest.getMainAttributes().put(Attributes.Name.MAIN_CLASS, "TODO");
        manifest.getMainAttributes().put(Attributes.Name.SPECIFICATION_TITLE, engine.getFactory().getLanguageName());
        manifest.getMainAttributes().put(Attributes.Name.SPECIFICATION_VERSION,
                engine.getFactory().getLanguageVersion());
        manifest.getMainAttributes().put(Attributes.Name.IMPLEMENTATION_TITLE, engine.getFactory().getEngineName());
        manifest.getMainAttributes().put(Attributes.Name.IMPLEMENTATION_VERSION,
                engine.getFactory().getEngineVersion());
        return manifest;
    }
}
