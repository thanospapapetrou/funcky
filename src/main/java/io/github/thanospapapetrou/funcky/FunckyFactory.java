package io.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.script.ScriptEngineFactory;

import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;

public class FunckyFactory implements ScriptEngineFactory {
    private static final String CONFIG_ENGINE = "Engine: %1$s %2$s";
    private static final String CONFIG_EXTENSIONS = "Extensions: %1$s";
    private static final String CONFIG_LANGUAGE = "Language: %1$s %2$s";
    private static final String CONFIG_MIME_TYPES = "MIME Types: %1$s";
    private static final String CONFIG_NAMES = "Names: %1$s";
    private static final String CONFIG_THREADING = "Threading: %1$s";
    private static final String DELIMITER_PARAMETER = ",";
    private static final String DELIMITER_STATEMENT = "\n";
    private static final Logger LOGGER = Logger.getLogger(FunckyFactory.class.getName());
    private static final Properties PARAMETERS = new Properties();
    private static final String PROPERTY_JVM_NAME = "java.vm.name";
    private static final String PROPERTY_JVM_VERSION = "java.vm.version";
    private static final String PROPERTY_JVM_VENDOR = "java.vm.vendor";
    private static final String PROPERTY_OS_NAME = "os.name";
    private static final String PROPERTY_OS_VERSION = "os.version";
    private static final String PROPERTY_OS_ARCHITECTURE = "os.arch";
    private static final String PROPERTY_USERNAME = "user.name";

    private final Properties parameters;

    static {
        try (final InputStream parameters = FunckyFactory.class.getResourceAsStream("/funcky.properties")) {
            PARAMETERS.load(parameters);
        } catch (final IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public FunckyFactory() {
        this(new Properties(PARAMETERS));
        setParameter(FunckyEngine.PARAMETER_JVM_NAME, System.getProperty(PROPERTY_JVM_NAME));
        setParameter(FunckyEngine.PARAMETER_JVM_VERSION, System.getProperty(PROPERTY_JVM_VERSION));
        setParameter(FunckyEngine.PARAMETER_JVM_VENDOR, System.getProperty(PROPERTY_JVM_VENDOR));
        setParameter(FunckyEngine.PARAMETER_OS_NAME, System.getProperty(PROPERTY_OS_NAME));
        setParameter(FunckyEngine.PARAMETER_OS_VERSION, System.getProperty(PROPERTY_OS_VERSION));
        setParameter(FunckyEngine.PARAMETER_OS_ARCHITECTURE, System.getProperty(PROPERTY_OS_ARCHITECTURE));
        setParameter(FunckyEngine.PARAMETER_USERNAME, System.getProperty(PROPERTY_USERNAME));
        LOGGER.config(String.format(CONFIG_LANGUAGE, getLanguageName(), getLanguageVersion()));
        LOGGER.config(String.format(CONFIG_ENGINE, getEngineName(), getEngineVersion()));
        LOGGER.config(String.format(CONFIG_NAMES, getNames()));
        LOGGER.config(String.format(CONFIG_MIME_TYPES, getMimeTypes()));
        LOGGER.config(String.format(CONFIG_EXTENSIONS, getExtensions()));
        LOGGER.config(String.format(CONFIG_THREADING, getParameter(FunckyEngine.PARAMETER_THREADING)));
    }

    private FunckyFactory(final Properties parameters) {
        this.parameters = parameters;
    }

    @Override
    public String getLanguageName() {
        return getParameter(FunckyEngine.LANGUAGE);
    }

    @Override
    public String getLanguageVersion() {
        return getParameter(FunckyEngine.LANGUAGE_VERSION);
    }

    @Override
    public String getEngineName() {
        return getParameter(FunckyEngine.ENGINE);
    }

    @Override
    public String getEngineVersion() {
        return getParameter(FunckyEngine.ENGINE_VERSION);
    }

    @Override
    public List<String> getNames() {
        return getParameters(FunckyEngine.NAME);
    }

    @Override
    public List<String> getMimeTypes() {
        return getParameters(FunckyEngine.PARAMETER_MIME_TYPES);
    }

    @Override
    public List<String> getExtensions() {
        return getParameters(FunckyEngine.PARAMETER_EXTENSIONS);
    }

    @Override
    public String getParameter(final String key) {
        final List<String> parameters = getParameters(key);
        return parameters.isEmpty() ? null : parameters.getFirst();
    }

    public void setParameter(final String key, final String value) {
        if (PARAMETERS.containsKey(key)) {
            throw new IllegalArgumentException("Invalid key: " + key); // TODO
        }
        parameters.setProperty(key, value);
    }

    @Override
    public String getMethodCallSyntax(final String object, final String method, final String... arguments) {
        // TODO
        throw new UnsupportedOperationException();
    }

    @Override
    public String getOutputStatement(final String message) {
        // TODO
        throw new UnsupportedOperationException();
    }

    @Override
    public String getProgram(final String... statements) {
        return String.join(DELIMITER_STATEMENT, statements);
    }

    @Override
    public FunckyEngine getScriptEngine() {
        final FunckyEngine engine = new FunckyEngine(this);
        setParameters(engine, FunckyContext.GLOBAL_SCOPE, PARAMETERS);
        setParameters(engine, FunckyContext.ENGINE_SCOPE, parameters);
        return engine;
    }

    private List<String> getParameters(final String key) {
        final String parameters = this.parameters.getProperty(key);
        return (parameters == null) ? List.of() : List.of(parameters.split(DELIMITER_PARAMETER));
    }

    private void setParameters(final FunckyEngine engine, final int scope, final Properties parameters) {
        parameters.keySet().stream()
                .map(String.class::cast)
                .forEach(parameter -> engine.getBindings(scope).put(parameter, (getParameters(parameter).size() > 1)
                        ? engine.toFuncky(getParameters(parameter))
                        : engine.toFuncky(getParameter(parameter))));

    }
}
