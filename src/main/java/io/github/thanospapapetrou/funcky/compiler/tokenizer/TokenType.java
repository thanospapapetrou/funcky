package io.github.thanospapapetrou.funcky.compiler.tokenizer;

import java.util.regex.Pattern;

public enum TokenType {
    COMMENT("#.*", null),
    BINARY_NUMBER("(?<sign>[\\+\\-]?)0[Bb](?<value>[01]+)", Radix.BINARY),
    OCTAL_NUMBER("(?<sign>[\\+\\-]?)0(?<value>[0-7]+)", Radix.OCTAL),
    HEXADECIMAL_NUMBER("(?<sign>[\\+\\-]?)0[Xx](?<value>[0-9A-Fa-f]+)", Radix.HEXADECIMAL),
    DECIMAL_NUMBER("(?<sign>[\\+\\-]?)(?<value>(([0-9]+\\.[0-9]*)|(\\.[0-9]+)|([0-9]+))([E|e][\\+\\-]?[0-9]+)?)", Radix.DECIMAL),
    CHARACTER("'(?<value>[^'\\\\\t\b\n\r\f]|\\\\\\\\|\\\\t|\\\\b|\\\\n|\\\\r|\\\\f|\\\\'|\\\\\")'", null),
    OCTAL_CHARACTER("'\\\\(?<value>[0-3]?[0-7]{1,2})'", Radix.OCTAL),
    HEXADECIMAL_CHARACTER("'\\\\u(?<value>[0-9A-Fa-f]{4})'", Radix.HEXADECIMAL),
    STRING("\"(?<value>([^\"\\\\\t\b\n\r\f]|\\\\\\\\|\\\\t|\\\\b|\\\\n|\\\\r|\\\\f|\\\\'|\\\\\"|\\\\[0-3]?[0-7]{1,2}|\\\\u[0-9A-Fa-f]{4})*)\"", null),
    SYMBOL("(?<value>[A-Za-z_\\$][0-9A-Za-z_\\$]*)", null),
    SPACE(" ", null),
    COMMA("\\,", null),
    EQUAL("=", null),
    COLON(":", null),
    PERIOD("\\.", null),
    LEFT_PARENTHESIS("\\(", null),
    RIGHT_PARENTHESIS("\\)", null),
    LEFT_SQUARE_BRACKET("\\[", null),
    RIGHT_SQUARE_BRACKET("\\]", null),
    LEFT_CURLY_BRACKET("\\{", null),
    RIGHT_CURLY_BRACKET("\\}", null),
    EOL(null, null),
    EOF(null, null);

    public static final String GROUP_SIGN = "sign";
    public static final String GROUP_VALUE = "value";

    private final Pattern pattern;
    private final Radix radix;

    TokenType(final String pattern, final Radix radix) {
        this.pattern = (pattern == null) ? null : Pattern.compile(pattern);
        this.radix = radix;
    }

    public Pattern getPattern() {
        return pattern;
    }

    public Radix getRadix() {
        return radix;
    }
}
