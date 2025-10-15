package io.github.thanospapapetrou.funcky.compiler.transpiler;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.FunckyFactory;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.prelude.Combinators;

public class Transpiler {
    // TODO cleanup
    // TODO generate jar
    // TODO use flags from factory
    // TODO types, values, to string, equals, hashcode in transpiled code
    public static final String JAVA_PREFIX = "_";
    private static final String DELIMITER_EXTENSION = ".";
    private static final String EXTENSION_CLASS = "class";
    private static final String EXTENSION_JAR = "jar";
    private static final String EXTENSION_JAVA = "java";
    private static final String JAVA = """
            public class %1$s {
                final %2$s engine = new %3$s().getScriptEngine();
            
            %4$s    public static void main(final String[] arguments) throws %5$s {
                    var it = new %1$s();
                    %6$s.exit(((%7$s) ((%8$s) it.%9$s.%10$smain.eval(it.engine.getContext())).apply(new %11$s(it.engine, it.engine.getConverter().convert(%12$s.asList(arguments))), it.engine.getContext())).getValue().intValue());
                }
            
                %1$s() throws %5$s {
                }
            }
            """;
    private static final String MANIFEST_VERSION = "1.0";
    private static final String OPTION_OUTPUT = "-d";
    private static final String PATTERN_NON_WORD = "\\W+";

    private final FunckyEngine engine;
    private final JavaCompiler compiler;

    public Transpiler(final FunckyEngine engine) throws MalformedURLException {
        this(engine, ToolProvider.getSystemJavaCompiler()); //
        // TODO
    }

    private Transpiler(final FunckyEngine engine, final JavaCompiler compiler) {
        this.engine = engine;
        this.compiler = compiler;
    }

    public String getClass(final URI script) {
        return JAVA_PREFIX + String.join(JAVA_PREFIX, script.toString().split(PATTERN_NON_WORD));
    }

    public FunckyExpression transpile(final FunckyExpression expression) {
        if (expression == null) {
            return null;
        }
        final File source = generateJava(expression);
        //        compile(source);
        //        return load(source.getName().replace(EXTENSION_JAVA, ""));
        return null;
    }

    public FunckyScript transpile(final FunckyScript script) {
        final File java = generateJava(script);
        compile(java);
        packadze(script, java);
        return load(java, getClass(script.getFile()));
    }

    private File generateJava(final FunckyExpression expression) {
        return null;
    }

    private File generateJava(final FunckyScript script) {
        try {
            final File java = File.createTempFile(engine.getFactory().getNames().getFirst() + JAVA_PREFIX,
                    DELIMITER_EXTENSION + EXTENSION_JAVA, engine.getFactory().getTmpDir());
            // java.deleteOnExit(); TODO
            try (final FileWriter writer = new FileWriter(java, StandardCharsets.UTF_8)) {
                writer.write(
                        String.format(JAVA, getClass(java), FunckyEngine.class.getName(), FunckyFactory.class.getName(),
                        Stream.of(new Combinators(engine), script)// TODO resolve dependencies
                                .map(FunckyScript::toJava)
                                .collect(Collectors.joining()),
                        IOException.class.getName(), System.class.getSimpleName(), FunckyNumber.class.getName(),
                        FunckyFunction.class.getName(), getClass(script.getFile()), JAVA_PREFIX,
                        FunckyLiteral.class.getName(), Arrays.class.getName()));
                writer.flush();
            }
            return java;
        } catch (final IOException e) {
            throw new RuntimeException(e); // TODO
        }
    }

    private void compile(final File java) {
        final DiagnosticCollector<JavaFileObject> collector = new DiagnosticCollector<>();
        try (final StandardJavaFileManager manager = compiler.getStandardFileManager(collector, Locale.ROOT,
                StandardCharsets.UTF_8)) {
            if (!compiler.getTask(null, manager, collector,
                    List.of(OPTION_OUTPUT, engine.getFactory().getTmpDir().getPath()), null,
                    manager.getJavaFileObjectsFromFiles(List.of(java))).call()) {
                for (Diagnostic<? extends JavaFileObject> d : collector.getDiagnostics()) {
                    // TODO
                    System.out.println(d.getCode() + " " + d.getKind());
                    System.out.println(d.getMessage(Locale.ROOT));
                    System.out.println(d.getSource().getName() + " " + d.getLineNumber() + " " + d.getColumnNumber());
                    System.out.println(d.getPosition() + " " + d.getStartPosition() + " " + d.getEndPosition());
                }
                throw new IllegalStateException("Compilation failed"); // TODO
            }
        } catch (final IOException e) {
            throw new RuntimeException(e); // TODO
        }
    }

    private void packadze(final FunckyScript script, final File java) {
        final File jar = getJar(script.getFile());
        final String clazz = getClass(java) + DELIMITER_EXTENSION + EXTENSION_CLASS;
        try (final JarOutputStream output = new JarOutputStream(new FileOutputStream(jar), getManifest(java))) {
            output.putNextEntry(new ZipEntry(clazz));
            Files.copy(new File(engine.getFactory().getTmpDir(), clazz).toPath(), output);
            output.closeEntry();
            try (final JarInputStream input = new JarInputStream(
            engine.getClass().getProtectionDomain().getCodeSource().getLocation().openStream())) {
                JarEntry entry;
                while ((entry = input.getNextJarEntry()) != null) {
                    output.putNextEntry(new JarEntry(entry.getName()));
                    output.write(input.readAllBytes());
                    output.closeEntry();
                }
            }
            System.out.println("Created JAR: " + jar.getAbsolutePath());
        } catch (final IOException e) {
            throw new RuntimeException(e); // TODO
        }
        System.out.println("Engine is in: " + engine.getClass().getProtectionDomain().getCodeSource().getLocation());
    }

    private <T extends FunckyScript> T load(final File java, final String clazz) {
        try (final URLClassLoader loader =
                new URLClassLoader(new URL[]{engine.getFactory().getTmpDir().toURI().toURL()})) {
            final Class<T> s = (Class<T>) Arrays.stream(loader.loadClass(getClass(java)).getDeclaredClasses())
                    .filter(c -> c.getSimpleName().equals(clazz)).findAny()
                    .orElseThrow(() -> new RuntimeException("Error loading clazz: " + clazz));
            // TODO instantiate nested class
            final Constructor<T> constructor = s.getDeclaredConstructor(FunckyEngine.class);
            constructor.setAccessible(true);
            return constructor.newInstance(engine);
        } catch (final IOException | ReflectiveOperationException e) {
            throw new RuntimeException(e); // TODO
        }
        // TODO
        //             Class.class.getProtectionDomain().getCodeSource().getLocation();
    }

    private String getClass(final File java) {
        return java.getName().substring(0, java.getName().length() - (DELIMITER_EXTENSION + EXTENSION_JAVA).length());
    }

    private File getJar(final URI script) {
        return new File(engine.getFactory().getOutputDir(), getClass(script) + DELIMITER_EXTENSION + EXTENSION_JAR);
    }

    private Manifest getManifest(final File java) {
        final Manifest manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, MANIFEST_VERSION);
        manifest.getMainAttributes().put(Attributes.Name.MAIN_CLASS, getClass(java));
        manifest.getMainAttributes().put(Attributes.Name.SPECIFICATION_TITLE, engine.getFactory().getLanguageName());
        manifest.getMainAttributes()
                .put(Attributes.Name.SPECIFICATION_VERSION, engine.getFactory().getLanguageVersion());
        manifest.getMainAttributes().put(Attributes.Name.IMPLEMENTATION_TITLE, engine.getFactory().getEngineName());
        manifest.getMainAttributes()
                .put(Attributes.Name.IMPLEMENTATION_VERSION, engine.getFactory().getEngineVersion());
        return manifest;
    }
}
