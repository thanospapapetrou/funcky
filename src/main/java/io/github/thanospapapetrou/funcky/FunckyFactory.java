package io.github.thanospapapetrou.funcky;

import java.io.File;
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
    private static final String PARAMETERS = "/funcky.properties";
    private static final String SYSTEM_USER_DIR = "user.dir";
    private static final String SYSTEM_TMP_DIR = "java.io.tmpdir";

    private final Properties parameters;

    public FunckyFactory() throws IOException {
        this(new Properties(System.getProperties()));
        try (final InputStream parameters = FunckyFactory.class.getResourceAsStream(PARAMETERS)) {
            this.parameters.load(parameters);
        }
        LOGGER.config(String.format(CONFIG_LANGUAGE_NAME_VERSION, getLanguageName(), getLanguageVersion()));
        LOGGER.config(String.format(CONFIG_ENGINE_NAME_VERSION, getEngineName(), getEngineVersion()));
        LOGGER.config(String.format(CONFIG_NAMES, getNames()));
        LOGGER.config(String.format(CONFIG_MIME_TYPES, getMimeTypes()));
        LOGGER.config(String.format(CONFIG_EXTENSIONS, getExtensions()));
        LOGGER.config(String.format(CONFIG_THREADING, getParameter(FunckyEngine.PARAMETER_THREADING)));
        LOGGER.config(String.format(CONFIG_TRANSPILING, isTranspiling()));
        LOGGER.config(String.format(CONFIG_BASE_DIR, getBaseDir()));
        LOGGER.config(String.format(CONFIG_OUTPUT_DIR, getOutputDir()));
        LOGGER.config(String.format(CONFIG_TMP_DIR, getTmpDir()));
        LOGGER.config("");
    }

    private FunckyFactory(final Properties parameters) {
        this.parameters = parameters;
    }

    public boolean isTranspiling() {
        return Boolean.parseBoolean(parameters.getProperty(FunckyEngine.PARAMETER_TRANSPILING, Boolean.toString(false)));
    }

    public void setTranspiling(final boolean transpiling) {
        parameters.setProperty(FunckyEngine.PARAMETER_TRANSPILING, Boolean.toString(transpiling));
    }

    public File getBaseDir() throws IOException {
        return new File(parameters.getProperty(FunckyEngine.PARAMETER_BASE_DIR, System.getProperty(SYSTEM_USER_DIR)))
                .getCanonicalFile();
    }

    public void setBaseDir(final File base) throws IOException {
        parameters.setProperty(FunckyEngine.PARAMETER_BASE_DIR, base.getCanonicalPath());
    }

    public File getOutputDir() throws IOException {
        return new File(parameters.getProperty(FunckyEngine.PARAMETER_OUTPUT_DIR, System.getProperty(SYSTEM_USER_DIR)))
                .getCanonicalFile();
    }

    public void setOutputDir(final File output) throws IOException {
        parameters.setProperty(FunckyEngine.PARAMETER_OUTPUT_DIR, output.getCanonicalPath());
    }

    public File getTmpDir() throws IOException {
        return new File(parameters.getProperty(FunckyEngine.PARAMETER_TMP_DIR, System.getProperty(SYSTEM_TMP_DIR)))
                .getCanonicalFile();
    }

    public void setTmpDir(final File tmp) throws IOException {
        parameters.setProperty(FunckyEngine.PARAMETER_TMP_DIR, tmp.getCanonicalPath());
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
