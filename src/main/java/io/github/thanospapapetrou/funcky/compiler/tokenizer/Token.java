package io.github.thanospapapetrou.funcky.compiler.tokenizer;

import java.net.URI;
import java.util.regex.Matcher;

import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;

public record Token(TokenType type, String value, URI file, int line, int column) {
    private static final String FORMAT = "%1$s `%2$s`";

    public String signedValue() {
        final Matcher matcher = matcher();
        return matcher.group(TokenType.GROUP_SIGN) + matcher.group(TokenType.GROUP_VALUE);
    }

    public String unsignedValue() {
        return matcher().group(TokenType.GROUP_VALUE);
    }

    public String stringValue() {
        return EscapeHelper.unescape(unsignedValue());
    }

    @Override
    public String toString() {
        return (value == null) ? type.toString() : String.format(FORMAT, type, value);
    }

    private Matcher matcher() {
        final Matcher matcher = type.getPattern().matcher(value);
        matcher.matches();
        return matcher;
    }
}
