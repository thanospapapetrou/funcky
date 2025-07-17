package com.github.thanospapapetrou.funcky

import spock.lang.Specification
import spock.lang.Unroll

class FunckyFactorySpec extends Specification {
    private static final String[] ARGUMENTS = ['argument', 'other argument']
    private static final String ENGINE_NAME = 'funcky'
    private static final String ENGINE_VERSION = '2.0.0-SNAPSHOT'
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
        factory.getParameter(FunckyEngine.MIME_TYPES) == MIME_TYPES[0]
        factory.getParameter(FunckyEngine.EXTENSIONS) == EXTENSIONS[0]
        factory.getParameter(FunckyEngine.THREADING) == MULTITHREADED
        factory.getParameter(INVALID_PARAMETER) == null
    }

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
        when:
        factory.getOutputStatement(MESSAGE)
        then:
        thrown(UnsupportedOperationException)
    }

    @Unroll('Test get program (statements: #statements)')
    def 'Test get program'(final String[] statements) {
        expect:
        factory.getProgram(statements) == statements.join(FunckyFactory.STATEMENT_DELIMITER)
        where:
        statements << [[] as String[], STATEMENTS]
    }

    def 'Test get script engine'() {
        expect:
        factory.scriptEngine
    }
}
