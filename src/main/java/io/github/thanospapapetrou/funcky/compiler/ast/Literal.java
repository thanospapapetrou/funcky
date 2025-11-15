package io.github.thanospapapetrou.funcky.compiler.ast;

import java.math.BigDecimal;
import java.net.URI;
import java.util.List;
import java.util.stream.Collectors;

import io.github.thanospapapetrou.funcky.compiler.parser.EscapeHelper;

public final class Literal extends Expression {
    private static final String DELIMITER = ", ";
    private static final String FORMAT_CHARACTER = "'%1$s'";
    private static final String FORMAT_LIST = "[%1$s]";
    private static final String FORMAT_RECORD = "{%1$s}";
    private static final String FORMAT_STRING = "\"%1$s\"";

    private final BigDecimal number;
    private final Character character;
    private final List<Expression> list;
    private final String string;
    private final List<Expression> record;

    public static Literal number(final URI file, final int line, final int column, final BigDecimal number) {
        return new Literal(file, line, column, number, null, null, null, null);
    }

    public static Literal character(final URI file, final int line, final int column, final Character character) {
        return new Literal(file, line, column, null, character, null, null, null);
    }

    public static Literal list(final URI file, final int line, final int column, final List<Expression> list) {
        return new Literal(file, line, column, null, null, list, null, null);
    }

    public static Literal string(final URI file, final int line, final int column, final String string) {
        return new Literal(file, line, column, null, null, null, string, null);
    }

    public static Literal record(final URI file, final int line, final int column, final List<Expression> record) {
        return new Literal(file, line, column, null, null, null, null, record);
    }

    private Literal(final URI file, final int line, final int column, final BigDecimal number,
            final Character character, final List<Expression> list, final String string,
            final List<Expression> record) {
        super(file, line, column);
        this.number = number;
        this.character = character;
        this.list = list;
        this.string = string;
        this.record = record;
    }

    public BigDecimal getNumber() {
        return number;
    }

    public Character getCharacter() {
        return character;
    }

    public List<Expression> getList() {
        return list;
    }

    public String getString() {
        return string;
    }

    public List<Expression> getRecord() {
        return record;
    }

    @Override
    public String toString() {
        return (number == null) ? ((character == null) ? ((list == null) ? ((string == null) ? ((record == null) ? null
                : String.format(FORMAT_RECORD, record.stream()
                        .map(Expression::toString)
                        .collect(Collectors.joining(DELIMITER))))
                : String.format(FORMAT_STRING, EscapeHelper.escape(string)))
                : String.format(FORMAT_LIST, list.stream()
                        .map(Expression::toString)
                        .collect(Collectors.joining(DELIMITER))))
                : String.format(FORMAT_CHARACTER, EscapeHelper.escape(character.toString())))
                : number.toString();
    }
}
