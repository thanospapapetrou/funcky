package io.github.thanospapapetrou.funcky.compiler.tokenizer

import spock.lang.Specification
import spock.lang.Unroll

class TokenSpec extends Specification {
    private static final int COLUMN = 0
    private static final URI FILE = URI.create('https://www.example.org/')
    private static final int LINE = 1
    private static final String VALUE = 'value'

    @Unroll('Test constructor (type: #type, value: #value)')
    def 'Test constructor'(final TokenType type, final String value) {
        when:
        final Token result = new Token(type, value, FILE, LINE, COLUMN)
        then:
        result
        result.value() == value
        result.file() == FILE
        result.line() == LINE
        result.column() == COLUMN
        where:
        [type, value] << [TokenType.values(), [null, VALUE]].combinations()
    }

    @Unroll('Test signed value (type: #type, value: #value)')
    def 'Test signed value'(final TokenType type, final String value, final String signed) {
        expect:
        new Token(type, value, FILE, LINE, COLUMN).signedValue() == signed
        where:
        [type, value, signed] << [
                (TokenType.BINARY_NUMBER)     : [
                        '0B0' : '0', '0B1': '1', '0B00': '00', '0B01': '01', '0B10': '10', '0B11': '11',
                        '0b0' : '0', '0b1': '1', '0b00': '00', '0b01': '01', '0b10': '10', '0b11': '11',
                        '+0B0': '+0', '+0B1': '+1', '+0B00': '+00', '+0B01': '+01', '+0B10': '+10', '+0B11': '+11',
                        '+0b0': '+0', '+0b1': '+1', '+0b00': '+00', '+0b01': '+01', '+0b10': '+10', '+0b11': '+11',
                        '-0B0': '-0', '-0B1': '-1', '-0B00': '-00', '-0B01': '-01', '-0B10': '-10', '-0B11': '-11',
                        '-0b0': '-0', '-0b1': '-1', '-0b00': '-00', '-0b01': '-01', '-0b10': '-10', '-0b11': '-11'
                ],
                (TokenType.OCTAL_NUMBER)      : [
                        '00' : '0', '07': '7', '000': '00', '007': '07', '070': '70', '077': '77',
                        '+00': '+0', '+07': '+7', '+000': '+00', '+007': '+07', '+070': '+70', '+077': '+77',
                        '-00': '-0', '-07': '-7', '-000': '-00', '-007': '-07', '-070': '-70', '-077': '-77'
                ],
                (TokenType.HEXADECIMAL_NUMBER): [
                        '0X0' : '0', '0XF': 'F', '0X00': '00', '0X0F': '0F', '0XF0': 'F0', '0XFF': 'FF',
                        '0x0' : '0', '0xf': 'f', '0x00': '00', '0x0f': '0f', '0xf0': 'f0', '0xff': 'ff',
                        '+0X0': '+0', '+0XF': '+F', '+0X00': '+00', '+0X0F': '+0F', '+0XF0': '+F0', '+0XFF': '+FF',
                        '+0x0': '+0', '+0xf': '+f', '+0x00': '+00', '+0x0f': '+0f', '+0xf0': '+f0', '+0xff': '+ff',
                        '-0X0': '-0', '-0XF': '-F', '-0X00': '-00', '-0X0F': '-0F', '-0XF0': '-F0', '-0XFF': '-FF',
                        '-0x0': '-0', '-0xf': '-f', '-0x00': '-00', '-0x0f': '-0f', '-0xf0': '-f0', '-0xff': '-ff'
                ],
                (TokenType.DECIMAL_NUMBER)    : [
                        '0.'   : '0.', '9.': '9.', '00.': '00.', '09.': '09.', '90.': '90.', '99.': '99.',
                        '0.0'  : '0.0', '9.9': '9.9', '00.0': '00.0', '09.9': '09.9', '90.0': '90.0', '99.9': '99.9',
                        '.0'   : '.0', '.9': '.9', '.00': '.00', '.09': '.09', '.90': '.90', '.99': '.99',
                        '0'    : '0', '9': '9', '00': '00', '09': '09', '90': '90', '99': '99',
                        '0E0'  : '0E0', '9E9': '9E9', '00E00': '00E00', '09E09': '09E09', '99E99': '99E99',
                        '0E+0' : '0E+0', '9E+9': '9E+9', '00E+00': '00E+00', '09E+09': '09E+09', '99E+99': '99E+99',
                        '0E-0' : '0E-0', '9E-9': '9E-9', '00E-00': '00E-00', '09E-09': '09E-09', '99E-99': '99E-99',
                        '0e0'  : '0e0', '9e9': '9e9', '00e00': '00e00', '09e09': '09e09', '99e99': '99e99',
                        '0e+0' : '0e+0', '9e+9': '9e+9', '00e+00': '00e+00', '09e+09': '09e+09', '99e+99': '99e+99',
                        '0e-0' : '0e-0', '9e-9': '9e-9', '00e-00': '00e-00', '09e-09': '09e-09', '99e-99': '99e-99',
                        '+0.'  : '+0.', '+9.': '+9.', '+00.': '+00.', '+09.': '+09.', '+90.': '+90.', '+99.': '+99.',
                        '+0.0' : '+0.0', '+9.9': '+9.9', '+00.0': '+00.0', '+09.9': '+09.9', '+90.0': '+90.0', '+99.9': '+99.9',
                        '+.0'  : '+.0', '+.9': '+.9', '+.00': '+.00', '+.09': '+.09', '+.90': '+.90', '+.99': '+.99',
                        '+0'   : '+0', '+9': '+9', '+00': '+00', '+09': '+09', '+90': '+90', '+99': '+99',
                        '+0E0' : '+0E0', '+9E9': '+9E9', '+00E00': '+00E00', '+09E09': '+09E09', '+99E99': '+99E99',
                        '+0E+0': '+0E+0', '+9E+9': '+9E+9', '+00E+00': '+00E+00', '+09E+09': '+09E+09', '+99E+99': '+99E+99',
                        '+0E-0': '+0E-0', '+9E-9': '+9E-9', '+00E-00': '+00E-00', '+09E-09': '+09E-09', '+99E-99': '+99E-99',
                        '+0e0' : '+0e0', '+9e9': '+9e9', '+00e00': '+00e00', '+09e09': '+09e09', '+99e99': '+99e99',
                        '+0e+0': '+0e+0', '+9e+9': '+9e+9', '+00e+00': '+00e+00', '+09e+09': '+09e+09', '+99e+99': '+99e+99',
                        '+0e-0': '+0e-0', '+9e-9': '+9e-9', '+00e-00': '+00e-00', '+09e-09': '+09e-09', '+99e-99': '+99e-99',
                        '-0.'  : '-0.', '-9.': '-9.', '-00.': '-00.', '-09.': '-09.', '-90.': '-90.', '-99.': '-99.',
                        '-0.0' : '-0.0', '-9.9': '-9.9', '-00.0': '-00.0', '-09.9': '-09.9', '-90.0': '-90.0', '-99.9': '-99.9',
                        '-.0'  : '-.0', '-.9': '-.9', '-.00': '-.00', '-.09': '-.09', '-.90': '-.90', '-.99': '-.99',
                        '-0'   : '-0', '-9': '-9', '-00': '-00', '-09': '-09', '-90': '-90', '-99': '-99',
                        '-0E0' : '-0E0', '-9E9': '-9E9', '-00E00': '-00E00', '-09E09': '-09E09', '-99E99': '-99E99',
                        '-0E+0': '-0E+0', '-9E+9': '-9E+9', '-00E+00': '-00E+00', '-09E+09': '-09E+09', '-99E+99': '-99E+99',
                        '-0E-0': '-0E-0', '-9E-9': '-9E-9', '-00E-00': '-00E-00', '-09E-09': '-09E-09', '-99E-99': '-99E-99',
                        '-0e0' : '-0e0', '-9e9': '-9e9', '-00e00': '-00e00', '-09e09': '-09e09', '-99e99': '-99e99',
                        '-0e+0': '-0e+0', '-9e+9': '-9e+9', '-00e+00': '-00e+00', '-09e+09': '-09e+09', '-99e+99': '-99e+99',
                        '-0e-0': '-0e-0', '-9e-9': '-9e-9', '-00e-00': '-00e-00', '-09e-09': '-09e-09', '-99e-99': '-99e-99',
                ]
        ].collectMany { token -> token.value.collect { [token.key, it.key, it.value] } }
    }

    @Unroll('Test unsigned value (type: #type, value: #value)')
    def 'Test unsigned value'(final TokenType type, final String value, final String unsigned) {
        expect:
        new Token(type, value, FILE, LINE, COLUMN).unsignedValue() == unsigned
        where:
        [type, value, unsigned] << [
                (TokenType.OCTAL_CHARACTER)      : ['\'\\0\'': '0', '\'\\7\'': '7', '\'\\000\'': '000', '\'\\377\'': '377'],
                (TokenType.HEXADECIMAL_CHARACTER): ['\'\\u0000\'': '0000', '\'\\uFFFF\'': 'FFFF', '\'\\uffff\'': 'ffff']
        ].collectMany { token -> token.value.collect { [token.key, it.key, it.value] } }
    }

    @Unroll('Test string value (type: #type, value: #value)')
    def 'Test string value'(final TokenType type, final String value, final String string) {
        expect:
        new Token(type, value, FILE, LINE, COLUMN).stringValue() == string
        where:
        [type, value, string] << [
                (TokenType.CHARACTER): ['\'a\'': 'a', '\'"\'': '"', '\'\\\\\'': '\\', '\'\\t\'': '\t', '\'\\b\'': '\b', '\'\\n\'': '\n', '\'\\r\'': '\r', '\'\\f\'': '\f', '\'\\\'\'': '\'', '\'\\"\'': '"'],
                (TokenType.STRING)   : [
                        '"a"'                                                            : 'a', '"\'"': '\'', '"\\\\"': '\\', '"\\t"': '\t', '"\\b"': '\b', '"\\n"': '\n', '"\\r"': '\r', '"\\f"': '\f', '"\\\'"': '\'', '"\\""': '"',
                        '"\\0"'                                                          : '\0', '"\\7"': '\7', '"\\377"': '\377',
                        '"\\u0000"'                                                      : '\u0000', '"\\u000F"': '\u000F', '"\\u000f"': '\u000f',
                        '"a\'\\\\\\t\\b\\n\\r\\f\\\'\\"\\0\\7\\377\\u0000\\u000F\\u000f"': 'a\'\\\t\b\n\r\f\'"\0\7\377\u0000\u000F\u000f'
                ],
                (TokenType.SYMBOL)   : ['A': 'A', 'a': 'a', '_': '_', '$': '$', 'A0Aa_$': 'A0Aa_$']
        ].collectMany { token -> token.value.collect { [token.key, it.key, it.value] } }
    }

    @Unroll('Test to string (type: #type, value: #value)')
    def 'Test to string'(final TokenType type, final String value) {
        expect:
        new Token(type, value, FILE, LINE, COLUMN).toString() == ((value == null) ? type.toString() : String.format(Token.FORMAT, type, VALUE))
        where:
        [type, value] << [TokenType.values(), [null, VALUE]].combinations()
    }
}
