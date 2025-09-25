package io.github.thanospapapetrou.funcky;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.script.ScriptContext;
import javax.script.ScriptEngineFactory;
import javax.script.SimpleBindings;
import javax.script.SimpleScriptContext;

public class FunckyFactory implements ScriptEngineFactory {
    public static final ScriptContext GLOBAL = new SimpleScriptContext();

    private static final String CONFIG_ENGINE_NAME_VERSION = "Engine: %1$s %2$s";
    private static final String CONFIG_EXTENSIONS = "Extensions: %1$s";
    private static final String CONFIG_LANGUAGE_NAME_VERSION = "Language: %1$s %2$s";
    private static final String CONFIG_MIME_TYPES = "MIME Types: %1$s";
    private static final String CONFIG_NAMES = "Names: %1$s";
    private static final String CONFIG_THREADING = "Threading: %1$s";
    private static final String DELIMITER_PARAMETER = ",";
    private static final String DELIMITER_STATEMENT = "\n";
    private static final Logger LOGGER = Logger.getLogger(FunckyFactory.class.getName());
    private static final Properties PARAMETERS = new Properties();

    static {
        GLOBAL.setBindings(new SimpleBindings(), ScriptContext.GLOBAL_SCOPE);
        try (final InputStream parameters = FunckyFactory.class.getResourceAsStream("/funcky.properties")) {
            PARAMETERS.load(parameters);
        } catch (final IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public FunckyFactory() {
        LOGGER.config(String.format(CONFIG_LANGUAGE_NAME_VERSION, getLanguageName(), getLanguageVersion()));
        LOGGER.config(String.format(CONFIG_ENGINE_NAME_VERSION, getEngineName(), getEngineVersion()));
        LOGGER.config(String.format(CONFIG_NAMES, getNames()));
        LOGGER.config(String.format(CONFIG_MIME_TYPES, getMimeTypes()));
        LOGGER.config(String.format(CONFIG_EXTENSIONS, getExtensions()));
        LOGGER.config(String.format(CONFIG_THREADING, getParameter(FunckyEngine.PARAMETER_THREADING)));
        LOGGER.config("");
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
        engine.setBindings(GLOBAL.getBindings(ScriptContext.GLOBAL_SCOPE), ScriptContext.GLOBAL_SCOPE);
        return engine;
    }

    private List<String> getParameters(final String key) {
        final String parameters = PARAMETERS.getProperty(key);
        return (parameters == null) ? List.of() : List.of(parameters.split(DELIMITER_PARAMETER));
    }
}
