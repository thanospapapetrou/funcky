package io.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.ScriptEngineFactory;

public class FunckyFactory implements ScriptEngineFactory {
    public static final FunckyEngine ENGINE = new FunckyFactory().getScriptEngine();

    private static final String CONFIG_ENGINE_NAME_VERSION = "Engine: %1$s %2$s";
    private static final String CONFIG_EXTENSIONS = "Extensions: %1$s";
    private static final String CONFIG_LANGUAGE_NAME_VERSION = "Language: %1$s %2$s";
    private static final String CONFIG_MIME_TYPES = "MIME Types: %1$s";
    private static final String CONFIG_NAMES = "Names: %1$s";
    private static final String CONFIG_THREADING = "Threading: %1$s";
    private static final String DELIMITER_PARAMETER = ",";
    private static final String DELIMITER_STATEMENT = "\n";
    private static final String ERROR_LOADING_PARAMETERS = "Error loading parameters";
    private static final String PARAMETERS = "/funcky.properties";

    private final Properties parameters;

    public FunckyFactory() {
        this(new Properties());
        final Logger logger = Logger.getLogger(FunckyFactory.class.getName());
        try (final InputStream parameters = FunckyFactory.class.getResourceAsStream(PARAMETERS)) {
            this.parameters.load(parameters);
            logger.config(String.format(CONFIG_LANGUAGE_NAME_VERSION, getLanguageName(), getLanguageVersion()));
            logger.config(String.format(CONFIG_ENGINE_NAME_VERSION, getEngineName(), getEngineVersion()));
            logger.config(String.format(CONFIG_NAMES, getNames()));
            logger.config(String.format(CONFIG_MIME_TYPES, getMimeTypes()));
            logger.config(String.format(CONFIG_EXTENSIONS, getExtensions()));
            logger.config(String.format(CONFIG_THREADING, getParameter(FunckyEngine.PARAMETER_THREADING)));
            logger.config("");
        } catch (final IOException e) {
            logger.log(Level.SEVERE, ERROR_LOADING_PARAMETERS, e);
            throw new ExceptionInInitializerError(e);
        }
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
        return parameters.isEmpty() ? null : parameters.get(0);
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

    private List<String> getParameters(final String key) {
        final String parameters = this.parameters.getProperty(key);
        return (parameters == null) ? List.of() : List.of(parameters.split(DELIMITER_PARAMETER));
    }
}
