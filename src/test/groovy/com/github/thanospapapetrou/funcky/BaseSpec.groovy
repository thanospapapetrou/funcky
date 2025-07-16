package com.github.thanospapapetrou.funcky

import com.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import com.github.thanospapapetrou.funcky.runtime.FunckyList
import com.github.thanospapapetrou.funcky.runtime.FunckyNumber
import javax.script.ScriptContext
import spock.lang.Shared
import spock.lang.Specification

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
    protected static final Map<String, FunckyNumber> NUMBERS = (BINARY_NUMBERS + OCTAL_NUMBERS + DECIMAL_NUMBERS + HEXADECIMAL_NUMBERS)
            .collectEntries { [(it.key): new FunckyNumber(it.value)] }
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
    protected static final Map<String, FunckyCharacter> CHARACTERS = (LITERAL_CHARACTERS + OCTAL_CHARACTERS + HEXADECIMAL_CHARACTERS)
            .collectEntries { [(it.key): new FunckyCharacter(it.value)] }
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

    protected Map<String, FunckyList> getStrings() {
        STRINGS.collectEntries { [(it.key): engine.converter.convert(it.value)] }
    }
}
