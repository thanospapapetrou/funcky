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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.script.CompiledScript;
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
    public static final String JAVA_PREFIX = "$";
    private static final String DELIMITER_EXTENSION = ".";
    private static final String EXTENSION_JAVA = "java";
    private static final String JAVA = """
            public class %1$s {
                final %2$s engine = new %3$s().getScriptEngine();
                
            %4$s
                public static void main(final String[] arguments) throws %5$s {
                    var it = new %1$s();
                    %6$s.exit(((%7$s) ((%8$s) it.%9$s.%10$smain).apply(new %11$s(it.engine, it.engine.getConverter().convert(%12$s.asList(arguments))), it.engine.getContext())).getValue().intValue());
                }
            
                %1$s() throws %5$s {
                }
            }
            """;
    private static final String OPTION_OUTPUT = "-d";
    private static final String PATTERN_NON_WORD = "\\W+";
    private static final String PREFIX_POSIX_HIDDEN = ".";
    private static final String PREFIX_UNDERSCORE = "_";
    private static final File USER_HOME;

    static {
        try {
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

    public String getClass(final URI file) {
        return JAVA_PREFIX + String.join(PREFIX_UNDERSCORE, file.toString().split(PATTERN_NON_WORD));
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
        return load(getClass(script.getFile()));
    }

    private File generateJava(final FunckyExpression expression) {
        return null;
    }

    private File generateJava(final FunckyScript script) {
        try {
            final File file = File.createTempFile(engine.getFactory().getNames().getFirst() + JAVA_PREFIX,
                    DELIMITER_EXTENSION + EXTENSION_JAVA);
            // file.deleteOnExit(); TODO
            try (final FileWriter writer = new FileWriter(file, StandardCharsets.UTF_8)) {
                final String clazz = file.getName()
                        .substring(0, file.getName().length() - (DELIMITER_EXTENSION + EXTENSION_JAVA).length());
                writer.write(String.format(JAVA, clazz, FunckyEngine.class.getName(), FunckyFactory.class.getName(),
                        Stream.of(new Combinators(engine), script)// TODO resolve dependencies
                                .map(scr -> scr.toJava(clazz))
                                .collect(Collectors.joining()),
                        IOException.class.getName(), System.class.getSimpleName(), FunckyNumber.class.getName(),
                        FunckyFunction.class.getName(), getClass(script.getFile()), JAVA_PREFIX,
                        FunckyLiteral.class.getName(), Arrays.class.getName()));
                writer.flush();
            }
            return file;
        } catch (final IOException e) {
            throw new RuntimeException(e); // TODO
        }
    }

    private void compile(final File source) {
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
        //        try {
        //            return (T) loader.loadClass(clazz).getDeclaredConstructor(FunckyEngine.class).newInstance(engine);
        //        } catch (final ReflectiveOperationException e) {
        //            throw new RuntimeException(e); // TODO
        //        }
        // TODO
        return null;
    }
}
