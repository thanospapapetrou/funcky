package io.github.thanospapapetrou.funcky.compiler.parser;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class EscapeHelper {
    private static final Map<Pattern, Character> ESCAPE_CHARACTERS = Map.of(
            "\\\\", '\\',
            "\\t", '\t',
            "\\b", '\b',
            "\\n", '\n',
            "\\r", '\r',
            "\\f", '\f',
            "\\'", '\'',
            "\\\"", '"'
    ).entrySet().stream().collect(Collectors.toMap(
            entry -> Pattern.compile(Pattern.quote(entry.getKey())),
            Map.Entry::getValue));
    private static final Map<Pattern, Integer> ESCAPE_SEQUENCE = Map.of(
            "\\\\(?<value>[0-3]?[0-7]{1,2})", 8,
            "\\\\u(?<value>[0-9A-Fa-f]{4})", 16
    ).entrySet().stream().collect(Collectors.toMap(
            entry -> Pattern.compile(entry.getKey()),
            Map.Entry::getValue));
    private static final String HEXADECIMAL_ESCAPE = "\\u%1$04x";
    private static final String VALUE = "value";

    public static String escape(final String unescaped) {
        return unescaped.isEmpty() ? ""
                : ((Character.isISOControl(unescaped.charAt(0)) ? String.format(HEXADECIMAL_ESCAPE,
                        (int) unescaped.charAt(0)) : unescaped.charAt(0)) + escape(unescaped.substring(1)));
    }

    public static String unescape(final String escaped) {
        if (escaped.isEmpty()) {
            return "";
        }
        for (final Map.Entry<Pattern, Character> escapeCharacter : ESCAPE_CHARACTERS.entrySet()) {
            final Matcher matcher = escapeCharacter.getKey().matcher(escaped);
            if (matcher.lookingAt()) {
                return escapeCharacter.getValue() + unescape(escaped.substring(matcher.group().length()));
            }
        }
        for (final Map.Entry<Pattern, Integer> escapeSequence : ESCAPE_SEQUENCE.entrySet()) {
            final Matcher matcher = escapeSequence.getKey().matcher(escaped);
            if (matcher.lookingAt()) {
                return ((char) Integer.parseInt(matcher.group(VALUE), escapeSequence.getValue()))
                        + unescape(escaped.substring(matcher.group(0).length()));
            }
        }
        return escaped.charAt(0) + unescape(escaped.substring(1));
    }
}
