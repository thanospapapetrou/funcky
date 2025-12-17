package io.github.thanospapapetrou.funcky


import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyList
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable
import spock.lang.Shared
import spock.lang.Specification

import javax.script.ScriptContext

abstract class BaseSpec extends Specification {
    protected static final Map<String, BigDecimal> BINARY_NUMBERS = [
            ['': 1.0G, '+': 1.0G, '-': -1.0G], // sign
            ['B', 'b'], // prefix
            ['0': 0.0G, '1': 1.0G, '01': 1.0G, '10': 2.0G] // value
    ].combinations().collectEntries { [(it[0].key + '0' + it[1] + it[2].key): it[0].value * it[2].value] }
    protected static final Map<String, BigDecimal> OCTAL_NUMBERS = [
            ['': 1.0G, '+': 1.0G, '-': -1.0G], // sign
            ['0': 0.0G, '1': 1.0G, '01': 1.0G, '10': 8.0G] // value
    ].combinations().collectEntries { [(it[0].key + '0' + it[1].key): it[0].value * it[1].value] }
    protected static final Map<String, BigDecimal> DECIMAL_NUMBERS = [
            ['': 1.0G, '+': 1.0G, '-': -1.0G], // sign
            ([ // value
               ['0': 0.0G, '1': 1.0G, '10': 10.0G], // integral part
               ['': 0.0G, '0': 0.0G, '1': 0.1G, '01': 0.01G, '10': 0.1G] // decimal part
            ].combinations()
                    .collectEntries { [(it[0].key + '.' + it[1].key): it[0].value + it[1].value] }
                    + ['.0': 0.0G, '.1': 0.1G, '.01': 0.01G, '.10': 0.1G] // decimal part only
                    + ['0': 0.0G, '1': 1.0G, '10': 10.0G]), // integral part only
            (['': 0] // no exponent
                    + [ // exponent
                        ['E', 'e'], // prefix
                        ['': 1, '+': 1, '-': -1], // sign
                        ['0': 0, '1': 1, '01': 1, '10': 10] // value
            ].combinations()
                    .collectEntries { [(it[0] + it[1].key + it[2].key): it[1].value * it[2].value] })].combinations()
            .collectEntries { [(it[0].key + it[1].key + it[2].key): it[0].value * it[1].value.scaleByPowerOfTen(it[2].value)] }
    protected static final Map<String, BigDecimal> HEXADECIMAL_NUMBERS = [
            ['': 1.0G, '+': 1.0G, '-': -1.0G], // sign
            ['0X', '0x'], // prefix
            ['0': 0.0G, '1': 1.0G, 'F': 15.0G, 'f': 15.0G, '01': 1.0G, '10': 16.0G] // value
    ].combinations().collectEntries { [(it[0].key + it[1] + it[2].key): it[0].value * it[2].value] }
    protected static final Map<String, Character> LITERAL_CHARACTERS = [
            '\'A\''   : ('A' as char),
            '\'a\''   : ('a' as char),
            '\'\\\\\'': ('\\' as char),
            '\'\\t\'' : ('\t' as char),
            '\'\\b\'' : ('\b' as char),
            '\'\\n\'' : ('\n' as char),
            '\'\\r\'' : ('\r' as char),
            '\'\\f\'' : ('\f' as char),
            '\'\\\'\'': ('\'' as char),
            '\'\\"\'' : ('"' as char)]
    protected static final Map<String, Character> OCTAL_CHARACTERS = [
            '\'\\0\'' : (0 as char),
            '\'\\1\'' : (1 as char),
            '\'\\01\'': (1 as char),
            '\'\\10\'': (8 as char)]
    protected static final Map<String, Character> HEXADECIMAL_CHARACTERS = [
            '\'\\u0000\'': (0 as char),
            '\'\\u0001\'': (1 as char),
            '\'\\u000F\'': (15 as char),
            '\'\\u000f\'': (15 as char),
            '\'\\u0010\'': (16 as char)]
    protected static final Map<String, String> STRINGS = [
            '""'                                              : '',
            '" "'                                             : ' ',
            '"A"'                                             : 'A',
            '"a"'                                             : 'a',
            '"\\\\"'                                          : '\\',
            '"\\t"'                                           : '\t',
            '"\\b"'                                           : '\b',
            '"\\n"'                                           : '\n',
            '"\\r"'                                           : '\r',
            '"\\f"'                                           : '\f',
            '"\\\'"'                                          : '\'',
            '"\\""'                                           : '"',
            '"\\0"'                                           : '\0',
            '"\\1"'                                           : '\1',
            '"\\01"'                                          : '\1',
            '"\\10"'                                          : '\10',
            '"\\u0000"'                                       : '\u0000',
            '"\\u0001"'                                       : '\u0001',
            '"\\u000F"'                                       : '\u000F',
            '"\\u000f"'                                       : '\u000F',
            '"\\u0010"'                                       : '\u0010',
            '" A\\\\\\t\\b\\n\\r\\f\\\'\\"\\10\\u000F\\u000f"': ' A\\\t\b\n\r\f\'\"\10\u000F\u000F',
            '"\\\\t"'                                         : '\\t',
            '"\\\\\\t"'                                       : '\\\t',
            '"\\\\\\\\t"'                                     : '\\\\t',
            '"\\\\\\\\\\t"'                                   : '\\\\\t',
            '"\\\\\\\\\\\\t"'                                 : '\\\\\\t',
    ]

    @Shared
    protected FunckyEngine engine

    def setupSpec() {
        engine = new FunckyFactory().scriptEngine
    }

    protected Reader setScript(final String script, final String... arguments) {
        engine.getBindings(ScriptContext.ENGINE_SCOPE).put(FunckyEngine.FILENAME, new File(System.getProperty('user.dir')).getCanonicalFile().toURI().relativize(getClass().getResource(script).toURI()).toString())
        engine.getBindings(ScriptContext.ENGINE_SCOPE).put(FunckyEngine.ARGV, arguments)
        new InputStreamReader(getClass().getResourceAsStream(script))
    }

    protected Map<String, FunckyNumber> getNumbers() {
        (BINARY_NUMBERS + OCTAL_NUMBERS + DECIMAL_NUMBERS + HEXADECIMAL_NUMBERS)
                .collectEntries { [(it.key): new FunckyNumber(engine, it.value)] }
    }

    protected Map<String, FunckyCharacter> getCharacters() {
        (LITERAL_CHARACTERS + OCTAL_CHARACTERS + HEXADECIMAL_CHARACTERS)
                .collectEntries { [(it.key): new FunckyCharacter(engine, it.value)] }
    }

    protected Map<String, FunckyList> getStrings() {
        STRINGS.collectEntries { [(it.key): toFuncky(it.value)] }
    }

    protected FunckyValue toFuncky(final Object object) {
        switch (object.class) {
            case FunckyValue:
                return (object as FunckyValue)
            case BigDecimal:
                return toFuncky(object as BigDecimal)
            case Character:
                return toFuncky(object as char)
            default: // TODO
                throw new IllegalStateException("Don't know how to convert `${object}` (${object.class}) to funcky")
        }
    }

    protected FunckyNumber toFuncky(final BigDecimal number) {
        new FunckyNumber(engine, number)
    }

    protected FunckyCharacter toFuncky(final char character) {
        new FunckyCharacter(engine, character)
    }

    protected FunckyList toFuncky(final List<?> list) {
        new FunckyList(engine,
                FunckyListType.LIST { list.isEmpty() ? new FunckyTypeVariable(engine) : toFuncky(list.first).type }.apply(engine),
                list.isEmpty() ? null : new FunckyLiteral(engine, toFuncky(list.first)),
                list.isEmpty() ? null : new FunckyLiteral(engine, toFuncky(list.subList(1, list.size()))))
    }

    protected FunckyList toFuncky(final String string) {
        toFuncky(string.toCharArray().collect() as List)
    }
}
