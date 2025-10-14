package io.github.thanospapapetrou.funcky;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.script.ScriptEngineFactory;

public class FunckyFactory implements ScriptEngineFactory {
    private static final String CONFIG_CURRENT_DIR = "Current Directory: %1$s";
    private static final String CONFIG_ENGINE_NAME_VERSION = "Engine: %1$s %2$s";
    private static final String CONFIG_EXTENSIONS = "Extensions: %1$s";
    private static final String CONFIG_LANGUAGE_NAME_VERSION = "Language: %1$s %2$s";
    private static final String CONFIG_MIME_TYPES = "MIME Types: %1$s";
    private static final String CONFIG_NAMES = "Names: %1$s";
    private static final String CONFIG_THREADING = "Threading: %1$s";
    private static final String CONFIG_TMP_DIR = "Temporary Directory: %1$s";
    private static final String CONFIG_TRANSPILING = "Transpiling: %1$s";
    private static final String DELIMITER_PARAMETER = ",";
    private static final String DELIMITER_STATEMENT = "\n";
    private static final File DIR_CURRENT;
    private static final File DIR_TMP;
    private static final Logger LOGGER = Logger.getLogger(FunckyFactory.class.getName());
    private static final String PARAMETERS = "/funcky.properties";

    private final Properties parameters;

    static {
        try {
            DIR_CURRENT = new File(System.getProperty("user.dir")).getCanonicalFile();
            DIR_TMP = new File(System.getProperty("java.io.tmpdir")).getCanonicalFile();
        } catch (final IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public FunckyFactory() throws IOException {
        this(new Properties());
        try (final InputStream parameters = FunckyFactory.class.getResourceAsStream(PARAMETERS)) {
            this.parameters.load(parameters);
            this.parameters.setProperty(FunckyEngine.PARAMETER_CURRENT_DIR, DIR_CURRENT.getPath());
            this.parameters.setProperty(FunckyEngine.PARAMETER_TMP_DIR, DIR_TMP.getPath());
        }
        LOGGER.config(String.format(CONFIG_LANGUAGE_NAME_VERSION, getLanguageName(), getLanguageVersion()));
        LOGGER.config(String.format(CONFIG_ENGINE_NAME_VERSION, getEngineName(), getEngineVersion()));
        LOGGER.config(String.format(CONFIG_NAMES, getNames()));
        LOGGER.config(String.format(CONFIG_MIME_TYPES, getMimeTypes()));
        LOGGER.config(String.format(CONFIG_EXTENSIONS, getExtensions()));
        LOGGER.config(String.format(CONFIG_THREADING, getParameter(FunckyEngine.PARAMETER_THREADING)));
        LOGGER.config(String.format(CONFIG_TRANSPILING, getParameter(FunckyEngine.PARAMETER_TRANSPILING)));
        LOGGER.config(String.format(CONFIG_CURRENT_DIR, getParameter(FunckyEngine.PARAMETER_CURRENT_DIR)));
        LOGGER.config(String.format(CONFIG_TMP_DIR, getParameter(FunckyEngine.PARAMETER_TMP_DIR)));
        LOGGER.config("");
    }

    private FunckyFactory(final Properties parameters) {
        this.parameters = parameters;
    }

    public boolean isTranspiling() {
        return Boolean.parseBoolean(getParameter(FunckyEngine.PARAMETER_TRANSPILING));
    }

    public void setTranspiling(final boolean transpiling) {
        parameters.setProperty(FunckyEngine.PARAMETER_TRANSPILING, Boolean.toString(transpiling));
    }

    public File getTmpDir() throws IOException {
        return new File(getParameter(FunckyEngine.PARAMETER_TMP_DIR)).getCanonicalFile();
    }

    public void setTmpDir(final File tmpDir) throws IOException {
        parameters.setProperty(FunckyEngine.PARAMETER_TMP_DIR, tmpDir.getCanonicalPath());
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
        return new FunckyEngine(this);
    }

    private List<String> getParameters(final String key) {
        final String parameters = this.parameters.getProperty(key);
        return (parameters == null) ? List.of() : List.of(parameters.split(DELIMITER_PARAMETER));
    }
}
