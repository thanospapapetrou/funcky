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
        // TODO simplify the next two
        PARAMETERS.keySet().stream()
                .map(String.class::cast)
                .forEach(parameter -> engine.getBindings(FunckyContext.GLOBAL_SCOPE).put(parameter,
                        getParameters(parameter).size() > 1 ? engine.toFuncky(getParameters(parameter))
                                : engine.toFuncky(getParameter(parameter))));
        parameters.keySet().stream()
                .map(String.class::cast)
                .forEach(parameter -> engine.getBindings(FunckyContext.ENGINE_SCOPE).put(parameter,
                        getParameter(parameter)));
        // TODO
        // PARAMETER_JVM_NAME
        // PARAMETER_JVM_VERSION
        // PARAMETER_JVM_VENDOR
        // PARAMETER_OS_NAME
        // PARAMETER_OS_VERSION
        // PARAMETER_OS_ARCHITECTURE
        return engine;
    }

    private List<String> getParameters(final String key) {
        final String parameters = this.parameters.getProperty(key);
        return (parameters == null) ? List.of() : List.of(parameters.split(DELIMITER_PARAMETER));
    }
}
