package io.github.thanospapapetrou.funcky

import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper
import spock.lang.Specification
import spock.lang.Unroll

class FunckyFactorySpec extends Specification {
    private static final String[] ARGUMENTS = ['argument', 'other argument']
    private static final String ENGINE_NAME = 'funcky'
    private static final String ENGINE_VERSION = '1.1.0-SNAPSHOT'
    private static final List<String> EXTENSIONS = ['funcky', 'fun']
    private static final String INVALID_PARAMETER = 'foo'
    private static final String LANGUAGE_NAME = 'Funcky'
    private static final String LANGUAGE_VERSION = '1.0'
    private static final String MESSAGE = 'message'
    private static final String METHOD = 'method'
    private static final List<String> MIME_TYPES = ['text/prs.funcky', 'text/x.funcky']
    private static final String MULTITHREADED = 'MULTITHREADED'
    private static final List<String> NAMES = ['Funcky', 'funcky']
    private static final String OBJECT = 'object'
    private static final String[] STATEMENTS = ['statement', 'other statement']

    private FunckyFactory factory

    def setup() {
        factory = new FunckyFactory()
    }

    def 'Test parameters'() {
        expect:
        factory.languageName == LANGUAGE_NAME
        factory.languageVersion == LANGUAGE_VERSION
        factory.engineName == ENGINE_NAME
        factory.engineVersion == ENGINE_VERSION
        factory.names == NAMES
        factory.mimeTypes == MIME_TYPES
        factory.extensions == EXTENSIONS
        factory.getParameter(FunckyEngine.LANGUAGE) == LANGUAGE_NAME
        factory.getParameter(FunckyEngine.LANGUAGE_VERSION) == LANGUAGE_VERSION
        factory.getParameter(FunckyEngine.ENGINE) == ENGINE_NAME
        factory.getParameter(FunckyEngine.ENGINE_VERSION) == ENGINE_VERSION
        factory.getParameter(FunckyEngine.NAME) == NAMES[0]
        factory.getParameter(FunckyEngine.PARAMETER_MIME_TYPES) == MIME_TYPES[0]
        factory.getParameter(FunckyEngine.PARAMETER_EXTENSIONS) == EXTENSIONS[0]
        factory.getParameter(FunckyEngine.PARAMETER_THREADING) == MULTITHREADED
        factory.getParameter(INVALID_PARAMETER) == null
        // TODO test for non-global parameters
    }

    // TODO test set parameters

    @Unroll('Test get method call syntax (arguments: #arguments)')
    def 'Test get method call syntax'(final String[] arguments) {
        when:
        factory.getMethodCallSyntax(OBJECT, METHOD, arguments)
        then:
        thrown(UnsupportedOperationException)
        where:
        arguments << [[] as String[], ARGUMENTS]
    }

    def 'Test get output statement'() {
        expect:
        factory.getOutputStatement(MESSAGE) == "\"funcky:io\".bind (\"funcky:io\".writeString \"funcky:io\".STDOUT \"${EscapeHelper.escape(MESSAGE)}\") (\"funcky:combinators\".k (\"funcky:io\".flush \"funcky:io\".STDOUT))"
    }

    @Unroll('Test get program (statements: #statements)')
    def 'Test get program'(final String[] statements) {
        expect:
        factory.getProgram(statements) == statements.join(FunckyFactory.DELIMITER_STATEMENT)
        where:
        statements << [[] as String[], STATEMENTS]
    }

    def 'Test get script engine'() {
        expect:
        factory.scriptEngine
    }
}
