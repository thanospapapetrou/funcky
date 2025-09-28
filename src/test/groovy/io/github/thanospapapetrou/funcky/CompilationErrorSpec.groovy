package io.github.thanospapapetrou.funcky

import io.github.thanospapapetrou.funcky.compiler.linker.Linker
import io.github.thanospapapetrou.funcky.compiler.exceptions.IllegalApplicationException
import io.github.thanospapapetrou.funcky.compiler.exceptions.InvalidMainException
import io.github.thanospapapetrou.funcky.compiler.exceptions.NameAlreadyDefinedException
import io.github.thanospapapetrou.funcky.compiler.exceptions.PrefixAlreadyBoundException
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnboundPrefixException
import io.github.thanospapapetrou.funcky.compiler.exceptions.UndefinedMainException
import io.github.thanospapapetrou.funcky.compiler.exceptions.UndefinedNameException
import io.github.thanospapapetrou.funcky.compiler.exceptions.InvalidListLiteralException
import io.github.thanospapapetrou.funcky.compiler.exceptions.InvalidUriException
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnexpectedTokenException
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Token
import io.github.thanospapapetrou.funcky.compiler.tokenizer.TokenType
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnrecognizedInputException
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType
import io.github.thanospapapetrou.funcky.runtime.FunckyListType
import io.github.thanospapapetrou.funcky.runtime.FunckyType
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType.FUNCTION
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.LIST
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.STRING
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.BOOLEAN
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.NUMBER

class CompilationErrorSpec extends BaseSpec {
    @Unroll('Test unrecognized input error in expression (expression: #expression)')
    def 'Test unrecognized input error in expression'(final String expression, final String unrecognized, final int column) {
        when:
        engine.eval(expression)
        then:
        final UnrecognizedInputException e = thrown()
        e.message
        e.message.startsWith(String.format(UnrecognizedInputException.MESSAGE, unrecognized))
        e.fileName == engine.linker.stdin.toString()
        e.lineNumber == 1
        e.columnNumber == column
        where:
        expression     || unrecognized | column
        '!'            || '!'          | 1
        '0B0!'         || '!'          | 4
        '00!'          || '!'          | 3
        '0X0!'         || '!'          | 4
        '1!'           || '!'          | 2
        '\'a\'!'       || '!'          | 4
        '\'\\0\'!'     || '!'          | 5
        '\'\\u0000\'!' || '!'          | 9
        '"a"!'         || '!'          | 4
        'foo!'         || '!'          | 4
        'foo !'        || '!'          | 5
        '[foo,!'       || '!'          | 6
        '"foo".!'      || '!'          | 7
        '[!'           || '!'          | 2
        '[foo]!'       || '!'          | 6
        '{!'           || '!'          | 2
        '{foo}!'       || '!'          | 6
    }

    @Unroll('Test unrecognized input error in script (script: #script)')
    def 'Test unrecognized input error in script'(final String script, final String unrecognized, final int line, final int column) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final UnrecognizedInputException e = thrown()
        e.message
        e.message.startsWith(String.format(UnrecognizedInputException.MESSAGE, unrecognized))
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == line
        e.columnNumber == column
        cleanup:
        reader.close()
        where:
        script                                                 || unrecognized | line | column
        '/compilation_error/unrecognized_input_error_1.funcky' || '!'          | 1    | 1
        '/compilation_error/unrecognized_input_error_2.funcky' || '!'          | 2    | 4
    }

    @Unroll('Test unexpected token error in expression (expression: #expression)')
    def 'Test unexpected token error in expression'(final String expression, final TokenType type, final String value, final List<TokenType> expected, final int column) {
        when:
        engine.eval(expression)
        then:
        final UnexpectedTokenException e = thrown()
        e.message
        e.message.startsWith(String.format(UnexpectedTokenException.MESSAGE, (value == null) ? type : String.format(Token.FORMAT, type, value), expected*.toString().join(UnexpectedTokenException.DELIMITER)))
        e.fileName == engine.linker.stdin.toString()
        e.lineNumber == 1
        e.columnNumber == column
        where:
        expression       || type                            | value         | expected                                                                                                                                                                                                                                                                                                                               | column
        ' '              || TokenType.SPACE                 | null          | [TokenType.BINARY_NUMBER, TokenType.OCTAL_NUMBER, TokenType.HEXADECIMAL_NUMBER, TokenType.DECIMAL_NUMBER, TokenType.CHARACTER, TokenType.OCTAL_CHARACTER, TokenType.HEXADECIMAL_CHARACTER, TokenType.STRING, TokenType.SYMBOL, TokenType.LEFT_PARENTHESIS, TokenType.LEFT_SQUARE_BRACKET, TokenType.LEFT_CURLY_BRACKET, TokenType.EOL] | 1
        ','              || TokenType.COMMA                 | null          | [TokenType.BINARY_NUMBER, TokenType.OCTAL_NUMBER, TokenType.HEXADECIMAL_NUMBER, TokenType.DECIMAL_NUMBER, TokenType.CHARACTER, TokenType.OCTAL_CHARACTER, TokenType.HEXADECIMAL_CHARACTER, TokenType.STRING, TokenType.SYMBOL, TokenType.LEFT_PARENTHESIS, TokenType.LEFT_SQUARE_BRACKET, TokenType.LEFT_CURLY_BRACKET, TokenType.EOL] | 1
        '='              || TokenType.EQUAL                 | null          | [TokenType.BINARY_NUMBER, TokenType.OCTAL_NUMBER, TokenType.HEXADECIMAL_NUMBER, TokenType.DECIMAL_NUMBER, TokenType.CHARACTER, TokenType.OCTAL_CHARACTER, TokenType.HEXADECIMAL_CHARACTER, TokenType.STRING, TokenType.SYMBOL, TokenType.LEFT_PARENTHESIS, TokenType.LEFT_SQUARE_BRACKET, TokenType.LEFT_CURLY_BRACKET, TokenType.EOL] | 1
        ':'              || TokenType.COLON                 | null          | [TokenType.BINARY_NUMBER, TokenType.OCTAL_NUMBER, TokenType.HEXADECIMAL_NUMBER, TokenType.DECIMAL_NUMBER, TokenType.CHARACTER, TokenType.OCTAL_CHARACTER, TokenType.HEXADECIMAL_CHARACTER, TokenType.STRING, TokenType.SYMBOL, TokenType.LEFT_PARENTHESIS, TokenType.LEFT_SQUARE_BRACKET, TokenType.LEFT_CURLY_BRACKET, TokenType.EOL] | 1
        '.'              || TokenType.PERIOD                | null          | [TokenType.BINARY_NUMBER, TokenType.OCTAL_NUMBER, TokenType.HEXADECIMAL_NUMBER, TokenType.DECIMAL_NUMBER, TokenType.CHARACTER, TokenType.OCTAL_CHARACTER, TokenType.HEXADECIMAL_CHARACTER, TokenType.STRING, TokenType.SYMBOL, TokenType.LEFT_PARENTHESIS, TokenType.LEFT_SQUARE_BRACKET, TokenType.LEFT_CURLY_BRACKET, TokenType.EOL] | 1
        ')'              || TokenType.RIGHT_PARENTHESIS     | null          | [TokenType.BINARY_NUMBER, TokenType.OCTAL_NUMBER, TokenType.HEXADECIMAL_NUMBER, TokenType.DECIMAL_NUMBER, TokenType.CHARACTER, TokenType.OCTAL_CHARACTER, TokenType.HEXADECIMAL_CHARACTER, TokenType.STRING, TokenType.SYMBOL, TokenType.LEFT_PARENTHESIS, TokenType.LEFT_SQUARE_BRACKET, TokenType.LEFT_CURLY_BRACKET, TokenType.EOL] | 1
        ']'              || TokenType.RIGHT_SQUARE_BRACKET  | null          | [TokenType.BINARY_NUMBER, TokenType.OCTAL_NUMBER, TokenType.HEXADECIMAL_NUMBER, TokenType.DECIMAL_NUMBER, TokenType.CHARACTER, TokenType.OCTAL_CHARACTER, TokenType.HEXADECIMAL_CHARACTER, TokenType.STRING, TokenType.SYMBOL, TokenType.LEFT_PARENTHESIS, TokenType.LEFT_SQUARE_BRACKET, TokenType.LEFT_CURLY_BRACKET, TokenType.EOL] | 1
        '}'              || TokenType.RIGHT_CURLY_BRACKET   | null          | [TokenType.BINARY_NUMBER, TokenType.OCTAL_NUMBER, TokenType.HEXADECIMAL_NUMBER, TokenType.DECIMAL_NUMBER, TokenType.CHARACTER, TokenType.OCTAL_CHARACTER, TokenType.HEXADECIMAL_CHARACTER, TokenType.STRING, TokenType.SYMBOL, TokenType.LEFT_PARENTHESIS, TokenType.LEFT_SQUARE_BRACKET, TokenType.LEFT_CURLY_BRACKET, TokenType.EOL] | 1
        'foo\'a\''       || TokenType.CHARACTER             | '\'a\''       | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo\'\\0\''     || TokenType.OCTAL_CHARACTER       | '\'\\0\''     | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo\'\\u0000\'' || TokenType.HEXADECIMAL_CHARACTER | '\'\\u0000\'' | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo"bar"'       || TokenType.STRING                | '"bar"'       | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo,'           || TokenType.COMMA                 | null          | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo='           || TokenType.EQUAL                 | null          | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo:'           || TokenType.COLON                 | null          | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo('           || TokenType.LEFT_PARENTHESIS      | null          | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo)'           || TokenType.RIGHT_PARENTHESIS     | null          | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo['           || TokenType.LEFT_SQUARE_BRACKET   | null          | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo]'           || TokenType.RIGHT_SQUARE_BRACKET  | null          | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo{'           || TokenType.LEFT_CURLY_BRACKET    | null          | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
        'foo}'           || TokenType.RIGHT_CURLY_BRACKET   | null          | [TokenType.SPACE, TokenType.PERIOD, TokenType.EOL]                                                                                                                                                                                                                                                                                     | 4
    }

    @Unroll('Test unexpected token error in script (script: #script)')
    def 'Test unexpected token error in script'(final String script, final TokenType type, final String value, final List<TokenType> expected, final int line, final int column) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final UnexpectedTokenException e = thrown()
        e.message
        e.message.startsWith(String.format(UnexpectedTokenException.MESSAGE, (value == null) ? type : String.format(Token.FORMAT, type, value), expected*.toString().join(UnexpectedTokenException.DELIMITER)))
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == line
        e.columnNumber == column
        cleanup:
        reader.close()
        where:
        script                                               || type                | value   | expected                                         | line | column
        '/compilation_error/unexpected_token_error_1.funcky' || TokenType.SPACE     | null    | [TokenType.SYMBOL, TokenType.EOL, TokenType.EOF] | 1    | 1
        '/compilation_error/unexpected_token_error_2.funcky' || TokenType.CHARACTER | '\'a\'' | [TokenType.SPACE, TokenType.COLON]               | 2    | 4
    }

    @Unroll('Test invalid URI error in expression (expression: #expression)')
    def 'Test invalid URI error in expression'(final String expression, final String invalid, final int column) {
        when:
        engine.eval(expression)
        then:
        final InvalidUriException e = thrown()
        e.message
        e.message.startsWith(String.format(InvalidUriException.MESSAGE, invalid))
        e.fileName == engine.linker.stdin.toString()
        e.lineNumber == 1
        e.columnNumber == column
        where:
        expression          || invalid   | column
        '"foo bar".buz'     || 'foo bar' | 1
        'foo "bar buz".foo' || 'bar buz' | 5
    }

    @Unroll('Test invalid URI error in script (script: #script)')
    def 'Test invalid URI error in script'(final String script, final String invalid, final int line, final int column) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final InvalidUriException e = thrown()
        e.message
        e.message.startsWith(String.format(InvalidUriException.MESSAGE, invalid))
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == line
        e.columnNumber == column
        cleanup:
        reader.close()
        where:
        script                                          || invalid   | line | column
        '/compilation_error/invalid_uri_error_1.funcky' || 'foo bar' | 2    | 7
        '/compilation_error/invalid_uri_error_2.funcky' || 'bar buz' | 1    | 6
    }

    @Unroll('Test invalid list literal error in expression (expression: #expression)')
    def 'Test invalid list literal error in expression'(final String expression, final String head, final FunckyType headType, final String tail, final FunckyListType tailType, final int column) {
        when:
        engine.eval(expression)
        then:
        final InvalidListLiteralException e = thrown()
        e.message
        e.message.startsWith(String.format(InvalidListLiteralException.MESSAGE, head, headType, tail, tailType))
        e.fileName == engine.linker.stdin.toString()
        e.lineNumber == 1
        e.columnNumber == column
        where:
        expression                       || head                      | headType | tail    | tailType                                | column
        '[1, \'a\']'                     || '1'                       | NUMBER.apply(engine)  | '"a"'   | STRING.apply(engine)       | 2
        '["funcky:booleans".false, "a"]' || '"funcky:booleans".false' | BOOLEAN.apply(engine) | '["a"]' | LIST(STRING).apply(engine) | 2
    }

    @Unroll('Test invalid list literal error in script (script: #script)')
    def 'Test invalid list literal error in script'(final String script, final String head, final FunckyType headType, final String tail, final FunckyListType tailType, final int line, final int column) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final InvalidListLiteralException e = thrown()
        e.message
        e.message.startsWith(String.format(InvalidListLiteralException.MESSAGE, head, headType, tail, tailType))
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == line
        e.columnNumber == column
        cleanup:
        reader.close()
        where:
        script                                                   || head                      | headType              | tail    | tailType                   | line | column
        '/compilation_error/invalid_list_literal_error_1.funcky' || '1'                       | NUMBER.apply(engine)  | '"a"'   | STRING.apply(engine)       | 1    | 8
        '/compilation_error/invalid_list_literal_error_2.funcky' || '"funcky:booleans".false' | BOOLEAN.apply(engine) | '["a"]' | LIST(STRING).apply(engine) | 2    | 8
    }

    @Unroll('Test prefix already bound error (script: #script)')
    def 'Test prefix already bound error'(final String script, final String prefix, final int otherLine, final int line, final int column) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final PrefixAlreadyBoundException e = thrown()
        e.message
        e.message.startsWith(String.format(PrefixAlreadyBoundException.MESSAGE, prefix, otherLine))
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == line
        e.columnNumber == column
        cleanup:
        reader.close()
        where:
        script                                                   || prefix | otherLine | line | column
        '/compilation_error/prefix_already_bound_error_1.funcky' || 'foo'  | 1         | 2    | 1
        '/compilation_error/prefix_already_bound_error_2.funcky' || 'bar'  | 2         | 3    | 1
    }

    @Unroll('Test name already defined error (script: #script)')
    def 'Test name already defined error'(final String script, final String name, final int otherLine, final int line, final int column) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final NameAlreadyDefinedException e = thrown()
        e.message
        e.message.startsWith(String.format(NameAlreadyDefinedException.MESSAGE, name, otherLine))
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == line
        e.columnNumber == column
        cleanup:
        reader.close()
        where:
        script                                                   || name  | otherLine | line | column
        '/compilation_error/name_already_defined_error_1.funcky' || 'foo' | 1         | 2    | 1
        '/compilation_error/name_already_defined_error_2.funcky' || 'bar' | 2         | 4    | 1
    }

    @Unroll('Test unbound prefix error in expression (expression: #expression)')
    def 'Test unbound prefix error in expression'(final String expression, final String prefix, final int column) {
        when:
        engine.eval(expression)
        then:
        final UnboundPrefixException e = thrown()
        e.message
        e.message.startsWith(String.format(UnboundPrefixException.MESSAGE, prefix))
        e.fileName == engine.linker.stdin.toString()
        e.lineNumber == 1
        e.columnNumber == column
        where:
        expression                      || prefix | column
        'foo.bar'                       || 'foo'  | 1
        '"funcky:numbers".plus foo.bar' || 'foo'  | 23
    }

    @Unroll('Test unbound prefix error in script (script: #script)')
    def 'Test unbound prefix error in script'(final String script, final String prefix, final int line, final int column) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final UnboundPrefixException e = thrown()
        e.message
        e.message.startsWith(String.format(UnboundPrefixException.MESSAGE, prefix))
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == line
        e.columnNumber == column
        cleanup:
        reader.close()
        where:
        script                                             || prefix | line | column
        '/compilation_error/unbound_prefix_error_1.funcky' || 'bar'  | 1    | 7
        '/compilation_error/unbound_prefix_error_2.funcky' || 'bar'  | 2    | 19
    }

    @Unroll('Test undefined name error in expression (expression: #expression)')
    def 'Test undefined name error in expression'(final String expression, final String name, final String namespace, final int column) {
        when:
        engine.eval(expression)
        then:
        final UndefinedNameException e = thrown()
        e.message
        e.message.startsWith(String.format(UndefinedNameException.MESSAGE, name, namespace))
        e.fileName == engine.linker.stdin.toString()
        e.lineNumber == 1
        e.columnNumber == column
        where:
        expression                 || name  | namespace               | column
        '"funcky:numbers".foo'     || 'foo' | 'funcky:numbers'        | 1
        '"funcky:numbers".add foo' || 'foo' | engine.linker.stdin     | 22
    }

    @Unroll('Test undefined name error in script (script: #script)')
    def 'Test undefined name error in script'(final String script, final String name, final String namespace, final int line, final int column) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final UndefinedNameException e = thrown()
        e.message
        e.message.startsWith(String.format(UndefinedNameException.MESSAGE, name, namespace))
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == line
        e.columnNumber == column
        cleanup:
        reader.close()
        where:
        script                                             || name  | namespace                                                   | line | column
        '/compilation_error/undefined_name_error_1.funcky' || 'bar' | 'funcky:numbers'                                            | 1    | 7
        '/compilation_error/undefined_name_error_2.funcky' || 'bar' | CompilationErrorSpec.getResource(script).toURI().toString() | 3    | 19
    }

    @Unroll('Test illegal application error in expression (expression: #expression)')
    def 'Test illegal application error in expression'(final String expression, final String function, final FunckyFunctionType functionType, final String argument, final FunckyType argumentType, final int column) {
        when:
        engine.eval(expression)
        then:
        final IllegalApplicationException e = thrown()
        e.message
        e.message.startsWith(String.format(IllegalApplicationException.MESSAGE, function, functionType, argument, argumentType))
        e.fileName == engine.linker.stdin.toString()
        e.lineNumber == 1
        e.columnNumber == column
        where:
        expression                                       || function                 | functionType                                   | argument                  | argumentType              | column
        '"funcky:numbers".add "funcky:booleans".false'   || '"funcky:numbers".add'   | FUNCTION(NUMBER, NUMBER, NUMBER).apply(engine) | '"funcky:booleans".false' | BOOLEAN.apply(engine)     | 1
        '"funcky:numbers".add 1 "funcky:booleans".false' || '"funcky:numbers".add 1' | FUNCTION(NUMBER, NUMBER).apply(engine)         | '"funcky:booleans".false' | BOOLEAN.apply(engine)     | 1
    }

    @Unroll('Test illegal application error in script (script: #script)')
    def 'Test illegal application error in script'(final String script, final String function, final FunckyFunctionType functionType, final String argument, final FunckyType argumentType, final int line, final int column) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final IllegalApplicationException e = thrown()
        e.message
        e.message.startsWith(String.format(IllegalApplicationException.MESSAGE, function, functionType, argument, argumentType))
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == line
        e.columnNumber == column
        cleanup:
        reader.close()
        where:
        script                                                  || function               | functionType                                   | argument                  | argumentType          | line | column
        '/compilation_error/illegal_application_error_1.funcky' || '"funcky:numbers".add' | FUNCTION(NUMBER, NUMBER, NUMBER).apply(engine) | '"funcky:booleans".false' | BOOLEAN.apply(engine) | 1    | 7
        '/compilation_error/illegal_application_error_2.funcky' || 'numbers.add 1'        | FUNCTION(NUMBER, NUMBER).apply(engine)         | 'booleans.false'          | BOOLEAN.apply(engine) | 4    | 7
    }

    @Unroll('Test undefined main error (script: #script)')
    def 'Test undefined main error'(final String script) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final UndefinedMainException e = thrown()
        e.message
        e.message.startsWith(UndefinedMainException.MESSAGE)
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == -1
        e.columnNumber == -1
        cleanup:
        reader.close()
        where:
        script << ['/compilation_error/undefined_main_error_1.funcky', '/compilation_error/undefined_main_error_2.funcky']
    }

    @Unroll('Test invalid main error (script: #script)')
    def 'Test invalid main error'(final String script, final FunckyType type, final int line) {
        given:
        final Reader reader = setScript(script)
        when:
        engine.eval(reader)
        then:
        final InvalidMainException e = thrown()
        e.message
        e.message.startsWith(String.format(InvalidMainException.MESSAGE, type, Linker.MAIN_TYPE.apply(engine)))
        e.fileName == CompilationErrorSpec.getResource(script).toURI().toString()
        e.lineNumber == line
        e.columnNumber == 1
        cleanup:
        reader.close()
        where:
        script                                           || type                                   | line
        '/compilation_error/invalid_main_error_1.funcky' || BOOLEAN.apply(engine)                  | 1
        '/compilation_error/invalid_main_error_2.funcky' || FUNCTION(NUMBER, NUMBER).apply(engine) | 3
    }
}
