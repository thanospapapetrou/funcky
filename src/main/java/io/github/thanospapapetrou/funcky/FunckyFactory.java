package io.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.ScriptEngineFactory;

import io.github.thanospapapetrou.funcky.logging.FunckyLoggerFactory;

public class FunckyFactory implements ScriptEngineFactory {
    public static final FunckyEngine ENGINE = new FunckyFactory().getScriptEngine();

    private static final String ENGINE_NAME_VERSION = "Engine: %1$s %2$s";
    private static final String ERORR_LOADING_PARAMETERS = "Error loading parameters";
    private static final String EXTENSIONS = "Extensions: %1$s";
    private static final String LANGUAGE_NAME_VERSION = "Language: %1$s %2$s";
    private static final String MIME_TYPES = "MIME Types: %1$s";
    private static final String NAMES = "Names: %1$s";
    private static final String PARAMETERS = "/funcky.properties";
    private static final String PARAMETER_DELIMITER = ",";
    private static final String STATEMENT_DELIMITER = "\n";
    private static final String THREADING = "Threading: %1$s";

    private final Properties parameters;

    public FunckyFactory() {
        this(new Properties());
        final Logger logger = FunckyLoggerFactory.getLogger(FunckyFactory.class);
        try (final InputStream parameters = FunckyFactory.class.getResourceAsStream(PARAMETERS)) {
            this.parameters.load(parameters);
            logger.config(String.format(LANGUAGE_NAME_VERSION, getLanguageName(), getLanguageVersion()));
            logger.config(String.format(ENGINE_NAME_VERSION, getEngineName(), getEngineVersion()));
            logger.config(String.format(NAMES, getNames()));
            logger.config(String.format(MIME_TYPES, getMimeTypes()));
            logger.config(String.format(EXTENSIONS, getExtensions()));
            logger.config(String.format(THREADING, getParameter(FunckyEngine.THREADING)));
            logger.config("");
        } catch (final IOException e) {
            logger.log(Level.SEVERE, ERORR_LOADING_PARAMETERS, e);
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
        return getParameters(FunckyEngine.MIME_TYPES);
    }

    @Override
    public List<String> getExtensions() {
        return getParameters(FunckyEngine.EXTENSIONS);
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
        return String.join(STATEMENT_DELIMITER, statements);
    }

    @Override
    public FunckyEngine getScriptEngine() {
        return new FunckyEngine(this);
    }

    private List<String> getParameters(final String key) {
        final String parameters = this.parameters.getProperty(key);
        return (parameters == null) ? List.of() : List.of(parameters.split(PARAMETER_DELIMITER));
    }
}
