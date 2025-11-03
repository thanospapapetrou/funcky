package io.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.script.ScriptEngineFactory;

public class FunckyFactory implements ScriptEngineFactory {
    private static final String CONFIG_BASE_DIR = "Base Directory: %1$s";
    private static final String CONFIG_ENGINE_NAME_VERSION = "Engine: %1$s %2$s";
    private static final String CONFIG_EXTENSIONS = "Extensions: %1$s";
    private static final String CONFIG_LANGUAGE_NAME_VERSION = "Language: %1$s %2$s";
    private static final String CONFIG_MIME_TYPES = "MIME Types: %1$s";
    private static final String CONFIG_NAMES = "Names: %1$s";
    private static final String CONFIG_OUTPUT_DIR = "Output Directory: %1$s";
    private static final String CONFIG_THREADING = "Threading: %1$s";
    private static final String CONFIG_TMP_DIR = "Temporary Directory: %1$s";
    private static final String CONFIG_TRANSPILING = "Transpiling: %1$s";
    private static final String DELIMITER_PARAMETER = ",";
    private static final String DELIMITER_STATEMENT = "\n";
    private static final Logger LOGGER = Logger.getLogger(FunckyFactory.class.getName());
    private static final Properties PARAMETERS = new Properties();

    static {
        try (final InputStream parameters = FunckyFactory.class.getResourceAsStream("/funcky.properties")) {
            PARAMETERS.load(parameters);
            PARAMETERS.setProperty(FunckyEngine.PARAMETER_BASE_DIR, System.getProperty("user.dir"));
            PARAMETERS.setProperty(FunckyEngine.PARAMETER_OUTPUT_DIR, System.getProperty("user.dir"));
            PARAMETERS.setProperty(FunckyEngine.PARAMETER_TMP_DIR, System.getProperty("java.io.tmpdir"));
        } catch (final IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private final Properties parameters;

    public static List<String> getParameters(final String key) {
        final String parameters = PARAMETERS.getProperty(key);
        return (parameters == null) ? List.of() : List.of(parameters.split(DELIMITER_PARAMETER));
    }

    public FunckyFactory() {
        this(System.getProperties());
        LOGGER.config(String.format(CONFIG_LANGUAGE_NAME_VERSION, getLanguageName(), getLanguageVersion()));
        LOGGER.config(String.format(CONFIG_ENGINE_NAME_VERSION, getEngineName(), getEngineVersion()));
        LOGGER.config(String.format(CONFIG_NAMES, getNames()));
        LOGGER.config(String.format(CONFIG_MIME_TYPES, getMimeTypes()));
        LOGGER.config(String.format(CONFIG_EXTENSIONS, getExtensions()));
        LOGGER.config(String.format(CONFIG_THREADING, getParameter(FunckyEngine.PARAMETER_THREADING)));
        LOGGER.config(String.format(CONFIG_TRANSPILING, getParameter(FunckyEngine.PARAMETER_TRANSPILING)));
        LOGGER.config(String.format(CONFIG_BASE_DIR, getParameter(FunckyEngine.PARAMETER_BASE_DIR)));
        LOGGER.config(String.format(CONFIG_OUTPUT_DIR, getParameter(FunckyEngine.PARAMETER_OUTPUT_DIR)));
        LOGGER.config(String.format(CONFIG_TMP_DIR, getParameter(FunckyEngine.PARAMETER_TMP_DIR)));
        LOGGER.config("");
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
        final String parameter = parameters.isEmpty() ? null : parameters.get(0);
        switch (key) {
            case FunckyEngine.LANGUAGE:
            case FunckyEngine.LANGUAGE_VERSION:
            case FunckyEngine.ENGINE:
            case FunckyEngine.ENGINE_VERSION:
            case FunckyEngine.NAME:
            case FunckyEngine.PARAMETER_MIME_TYPES:
            case FunckyEngine.PARAMETER_EXTENSIONS:
            case FunckyEngine.PARAMETER_THREADING:
                return parameter;
            default:
                return this.parameters.getProperty(key, parameter);
        }
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
        return new FunckyEngine(this);
    }
}
