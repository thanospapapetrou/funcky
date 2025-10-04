package io.github.thanospapapetrou.funcky.compiler.transpiler;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileStore;
import java.nio.file.Files;
import java.nio.file.attribute.DosFileAttributeView;
import java.nio.file.attribute.PosixFileAttributeView;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.function.Predicate;

import javax.script.CompiledScript;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.runtime.prelude.FunckyLibrary;

public class Transpiler {
    public static final String JAVA_PREFIX = "$";
    private static final String DELIMITER_EXTENSION = ".";
    private static final String DELIMITER_PACKAGE = ".";
    private static final String EXTENSION_JAVA = "java";
    private static final String OPTION_OUTPUT = "-d";
    private static final String PATTERN_NON_WORD = "\\W";
    private static final String PREFIX_POSIX_HIDDEN = ".";
    private static final String PREFIX_UNDERSCORE = "_";
    private static final File TMP_DIR;
    private static final File USER_HOME;

    static {
        try {
            TMP_DIR = new File(System.getProperty("java.io.tmpdir")).getCanonicalFile();
            USER_HOME = new File(System.getProperty("user.home")).getCanonicalFile();
        } catch (final IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private final FunckyEngine engine;
    private final JavaCompiler compiler;
    private final URLClassLoader loader;

    private static File getOuputDir(final FunckyEngine engine) {
        try {
            final FileStore store = Files.getFileStore(USER_HOME.toPath());
            final File output = new File(USER_HOME,
                    (store.supportsFileAttributeView(PosixFileAttributeView.class) ? PREFIX_POSIX_HIDDEN : "") + engine
                            .getFactory().getNames().getFirst());
            output.mkdirs();
            if (store.supportsFileAttributeView(DosFileAttributeView.class)) {
                Files.getFileAttributeView(output.toPath(), DosFileAttributeView.class).setHidden(true);
            }
            return output;
        } catch (final IOException e) { // TODO
            throw new RuntimeException(e);
        }
    }

    public Transpiler(final FunckyEngine engine) throws MalformedURLException {
        this(engine, ToolProvider.getSystemJavaCompiler(),
                new URLClassLoader(new URL[]{getOuputDir(engine).toURI().toURL()})); //
        // TODO
    }

    private Transpiler(final FunckyEngine engine, final JavaCompiler compiler, final URLClassLoader loader) {
        this.engine = engine;
        this.compiler = compiler;
        this.loader = loader;
    }

    public String getClassName(final URI file) {
        return String.join(DELIMITER_PACKAGE, split(file));
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
        if (java != null) {
            compile(java);
        }
        return load(getClassName(script.getFile()));
    }

    private File generateJava(final FunckyExpression expression) {
        return null;
    }

    private File generateJava(final FunckyScript script) {
        try {
            final File java = getJavaFile(script.getFile());
            java.getParentFile().mkdirs();
            if (!java.createNewFile()) {
                System.out.println("File already exists"); // TODO
                return null;
            }
            try (final FileWriter writer = new FileWriter(java, StandardCharsets.UTF_8)) {
                writer.write(script.toJava(getPackage(script.getFile()), getSimpleName(script.getFile()),
                        getParent(script)));
                writer.flush();
            }
            return java;
        } catch (final IOException e) {
            throw new RuntimeException(e); // TODO
        }

        //        final File file = engine.getManager().getLoaded(script.getFile());
        //        final Class<? extends FunckyLibrary> library = Linker.getLibrary(script.getFile());
        //        try (final FileWriter writer = new FileWriter(file, StandardCharsets.UTF_8)) {
        //            writer.write(
        //                    String.format(TEMPLATES.get((library == null) ? FunckyScript.class : FunckyLibrary.class),
        //                            file.getName().replace(EXTENSION_JAVA, ""),
        //                            ((library == null) ? FunckyScript.class : library).getName(),
        //                            script.getFile(),
        //                            script.getDefinitions().stream()
        //                                    .map(this::generateSource)
        //                                    .collect(Collectors.joining(TEMPLATE_DELIMITER)))); // TODO escape URIs
        //            System.out.println(
        //                    String.format(TEMPLATES.get((library == null) ? FunckyScript.class : FunckyLibrary.class),
        //                            file.getName().replace(EXTENSION_JAVA, ""),
        //                            ((library == null) ? FunckyScript.class : library).getName(),
        //                            script.getFile(),
        //                            script.getDefinitions().stream()
        //                                    .filter(definition -> definition.getLine() != -1)
        //                                    .map(this::generateSource)
        //                                    .collect(Collectors.joining(TEMPLATE_DELIMITER)))); // TODO escape URIs
        //        } catch (final IOException e) {
        //            throw new RuntimeException(e); // TODO
        //        }
        //        return file;
    }

    private void compile(final File source) {
        System.out.println("Output: " + getOuputDir(engine));
        final DiagnosticCollector<JavaFileObject> collector = new DiagnosticCollector<>();
        try (final StandardJavaFileManager manager = compiler.getStandardFileManager(collector, Locale.ROOT,
                StandardCharsets.UTF_8)) {
            if (compiler.getTask(null, manager, collector, List.of(OPTION_OUTPUT, getOuputDir(engine).toString()), null,
                    manager.getJavaFileObjectsFromFiles(List.of(source))).call()) {
            } else {
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

    private <T extends CompiledScript> T load(final String clazz) {
        try {
            return (T) loader.loadClass(clazz).getDeclaredConstructor(FunckyEngine.class).newInstance(engine);
        } catch (final ReflectiveOperationException e) {
            throw new RuntimeException(e); // TODO
        }
    }

    private File getJavaFile(final URI file) { // TODO check if this and the following should instead get a script as
        // an argument
        return new File(TMP_DIR,
                engine.getFactory().getNames().getFirst() + File.separator + String.join(File.separator, split(file))
                        + DELIMITER_EXTENSION + EXTENSION_JAVA);
    }

    private String getPackage(final URI file) {
        return getClassName(file).substring(0, getClassName(file).lastIndexOf(DELIMITER_PACKAGE));
    }

    private String getSimpleName(final URI file) {
        return getClassName(file).substring(getClassName(file).lastIndexOf(DELIMITER_PACKAGE) + 1);
    }

    private Class<? extends FunckyScript> getParent(final FunckyScript script) {
        final Class<? extends FunckyLibrary> library = engine.getLinker().getLibrary(script.getFile());
        return (library == null) ? FunckyScript.class : library;
    }

    private List<String> split(final URI file) {
        return Arrays.stream(removeExtensions(file).split(PATTERN_NON_WORD)).filter(Predicate.not(String::isEmpty))
                .map(string -> Character.isJavaIdentifierStart(string.charAt(0)) ? string
                        : (PREFIX_UNDERSCORE + string)).toList();
    }

    private String removeExtensions(final URI file) {
        for (final String extension : engine.getFactory().getExtensions()) {
            if (file.toString().endsWith(DELIMITER_EXTENSION + extension)) {
                return file.toString()
                        .substring(0, file.toString().length() - (DELIMITER_EXTENSION + extension).length());
            }
        }
        return file.toString();
    }
}
