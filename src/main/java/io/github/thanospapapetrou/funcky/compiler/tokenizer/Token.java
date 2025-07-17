package io.github.thanospapapetrou.funcky.compiler.tokenizer;

import java.net.URI;
import java.util.regex.Matcher;

import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;

public class Token {
    private static final String TOKEN = "%1$s `%2$s`";

    private final TokenType type;
    private final String value;
    private final URI file;
    private final int line;
    private final int column;

    public Token(final TokenType type, final String value, final URI file, final int line, final int column) {
        this.type = type;
        this.value = value;
        this.file = file;
        this.line = line;
        this.column = column;
    }

    public TokenType getType() {
        return type;
    }

    public String getValue() {
        return value;
    }

    public String getSignedValue() {
        final Matcher matcher = getMatcher();
        return matcher.group(TokenType.SIGN) + matcher.group(TokenType.VALUE);
    }

    public String getUnsignedValue() {
        return getMatcher().group(TokenType.VALUE);
    }

    public String getStringValue() {
        return EscapeHelper.unescape(getUnsignedValue());
    }

    public URI getFile() {
        return file;
    }

    public int getLine() {
        return line;
    }

    public int getColumn() {
        return column;
    }

    @Override
    public String toString() {
        return (value == null) ? type.toString() : String.format(TOKEN, type, value);
    }

    private Matcher getMatcher() {
        final Matcher matcher = getType().getPattern().matcher(value);
        matcher.matches();
        return matcher;
    }
}
