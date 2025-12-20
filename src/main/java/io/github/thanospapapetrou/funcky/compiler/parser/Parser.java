package io.github.thanospapapetrou.funcky.compiler.parser;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.logging.Logger;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.parser.exceptions.InvalidUriException;
import io.github.thanospapapetrou.funcky.compiler.parser.exceptions.UnexpectedTokenException;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Token;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.TokenType;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public class Parser {
    private static final String DEFINITION = "%1$sDefinition `%2$s` %3$s %4$d 1";
    private static final String EXPRESSION = "%1$s%2$s `%3$s` %4$s %5$d %6$d";
    private static final Set<TokenType> FIRST = Set.of(
            TokenType.BINARY_NUMBER,
            TokenType.OCTAL_NUMBER,
            TokenType.DECIMAL_NUMBER,
            TokenType.HEXADECIMAL_NUMBER,
            TokenType.CHARACTER,
            TokenType.OCTAL_CHARACTER,
            TokenType.HEXADECIMAL_CHARACTER,
            TokenType.STRING,
            TokenType.SYMBOL,
            TokenType.LEFT_PARENTHESIS,
            TokenType.LEFT_SQUARE_BRACKET,
            TokenType.LEFT_CURLY_BRACKET
    );
    private static final String IMPORT = "%1$sImport `%2$s` %3$s %4$d 1";
    private static final String INDENTATION = "  ";
    private static final Logger LOGGER = Logger.getLogger(Parser.class.getName());
    private static final String SCRIPT = "Script %1$s 1 1";
    private static final String TYPE_VARIABLE = "$_";
    private static final String UNEXPECTED_TOKEN = "Unexpected token `%1$s`";

    private final FunckyEngine engine;

    private static Set<TokenType> union(final Set<TokenType> a, final Set<TokenType> b) {
        final Set<TokenType> result = new HashSet<>();
        result.addAll(a);
        result.addAll(b);
        return result;
    }

    private static void log(final FunckyExpression expression, final int indentation) {
        if (expression != null) {
            LOGGER.finer(
                    String.format(EXPRESSION, INDENTATION.repeat(indentation),
                            expression.getClass().getSimpleName(),
                            expression, expression.getFile(), expression.getLine(), expression.getColumn()));
            if (expression instanceof FunckyApplication) {
                log(((FunckyApplication) expression).getFunction(), indentation + 1);
                log(((FunckyApplication) expression).getArgument(), indentation + 1);
            }
        }
    }

    private static void log(final FunckyScript script) {
        LOGGER.finer(String.format(SCRIPT, script.getFile()));
        for (final FunckyImport imp : script.getImports()) {
            LOGGER.finer(String.format(IMPORT, INDENTATION, imp, imp.file(), imp.line()));
        }
        for (final FunckyDefinition definition : script.getDefinitions()) {
            LOGGER.finer(String.format(DEFINITION, INDENTATION, definition, definition.file(), definition.line()));
            log(definition.expression(), 2);
        }
    }

    public Parser(final FunckyEngine engine) {
        this.engine = engine;
    }

    public FunckyExpression parse(final Queue<Token> input) {
        final FunckyExpression expression =
                (peek(input, union(FIRST, Set.of(TokenType.EOL))).type() == TokenType.EOL) ? null
                        : parseComplexExpression(input, Set.of(TokenType.EOL));
        consume(input, TokenType.EOL);
        consume(input, TokenType.EOF);
        log(expression, 0);
        return expression;
    }

    public FunckyScript parse(final Queue<Token> input, final URI file) {
        final FunckyScript script = new FunckyScript(engine, file);
        while (true) {
            final Token token = consume(input, Set.of(TokenType.SYMBOL, TokenType.EOL, TokenType.EOF));
            switch (token.type()) {
                case SYMBOL:
                    if (consume(input, Set.of(TokenType.COLON, TokenType.SPACE)).type() == TokenType.COLON) {
                        consume(input, TokenType.SPACE);
                        final URI namespace = parseUri(consume(input, TokenType.STRING));
                        consume(input, TokenType.EOL);
                        script.getImports().add(new FunckyImport(token.file(), token.line(), token.value(),
                                namespace));
                    } else {
                        consume(input, TokenType.EQUAL);
                        consume(input, TokenType.SPACE);
                        final FunckyExpression expression = parseComplexExpression(input, Set.of(TokenType.EOL));
                        consume(input, TokenType.EOL);
                        script.getDefinitions().add(new FunckyDefinition(token.file(), token.line(),
                                token.value(), expression));
                    }
                    break;
                case EOL:
                    break;
                case EOF:
                    log(script);
                    return script;
            }
        }
    }

    private FunckyExpression parseComplexExpression(final Queue<Token> input, final Set<TokenType> follow) {
        FunckyExpression expression = parseSimpleExpression(input, follow);
        while (true) {
            if (peek(input, union(Set.of(TokenType.SPACE), follow)).type() == TokenType.SPACE) {
                consume(input, TokenType.SPACE);
                expression = new FunckyApplication(expression, parseSimpleExpression(input, follow));
            } else {
                return expression;
            }
        }
    }

    private FunckyExpression parseSimpleExpression(final Queue<Token> input, final Set<TokenType> follow) {
        final Token token = consume(input, FIRST);
        switch (token.type()) {
            case BINARY_NUMBER:
            case OCTAL_NUMBER:
            case HEXADECIMAL_NUMBER:
                return new FunckyLiteral(engine, token.file(), token.line(), token.column(),
                        new FunckyNumber(engine, new BigDecimal(new BigInteger(token.signedValue(),
                                token.type().getRadix().getRadix()))));
            case DECIMAL_NUMBER:
                return new FunckyLiteral(engine, token.file(), token.line(), token.column(),
                        new FunckyNumber(engine, new BigDecimal(token.value())));
            case CHARACTER:
                return new FunckyLiteral(engine, token.file(), token.line(), token.column(),
                        new FunckyCharacter(engine, token.stringValue().charAt(0)));
            case OCTAL_CHARACTER:
            case HEXADECIMAL_CHARACTER:
                return new FunckyLiteral(engine, token.file(), token.line(), token.column(),
                        new FunckyCharacter(engine, (char) Integer.parseInt(token.unsignedValue(),
                                token.type().getRadix().getRadix())));
            case STRING:
                if (peek(input, union(Set.of(TokenType.PERIOD, TokenType.SPACE), follow)).type()
                        == TokenType.PERIOD) {
                    consume(input, TokenType.PERIOD);
                    return new FunckyReference(engine, token.file(), token.line(), token.column(),
                            parseUri(token), consume(input, TokenType.SYMBOL).value());
                }
                return parseString(token.stringValue(), token);
            case SYMBOL:
                if (peek(input, union(Set.of(TokenType.PERIOD, TokenType.SPACE), follow)).type()
                        == TokenType.PERIOD) {
                    consume(input, TokenType.PERIOD);
                    return new FunckyReference(engine, token.file(), token.line(), token.column(),
                            token.value(),
                            consume(input, TokenType.SYMBOL).value());
                }
                return token.value().equals(TYPE_VARIABLE)
                        ? new FunckyLiteral(engine, token.file(), token.line(), token.column(),
                        new FunckyTypeVariable(engine))
                        : new FunckyReference(engine, token.file(), token.line(), token.column(),
                                token.value());
            case LEFT_PARENTHESIS:
                final FunckyExpression expression = parseComplexExpression(input, Set.of(TokenType.RIGHT_PARENTHESIS));
                consume(input, TokenType.RIGHT_PARENTHESIS);
                return expression;
            case LEFT_SQUARE_BRACKET:
                if (peek(input, union(FIRST, Set.of(TokenType.RIGHT_SQUARE_BRACKET))).type()
                        == TokenType.RIGHT_SQUARE_BRACKET) {
                    consume(input, TokenType.RIGHT_SQUARE_BRACKET);
                    return parseList(List.of(), token);
                }
                final List<FunckyExpression> elements = new ArrayList<>();
                while (true) {
                    elements.add(parseComplexExpression(input,
                            Set.of(TokenType.COMMA, TokenType.RIGHT_SQUARE_BRACKET)));
                    if (consume(input, Set.of(TokenType.COMMA, TokenType.RIGHT_SQUARE_BRACKET)).type()
                            == TokenType.RIGHT_SQUARE_BRACKET) {
                        return parseList(elements, token);
                    }
                    consume(input, TokenType.SPACE);
                }
            case LEFT_CURLY_BRACKET:
                if (peek(input, union(FIRST, Set.of(TokenType.RIGHT_CURLY_BRACKET))).type()
                        == TokenType.RIGHT_CURLY_BRACKET) {
                    consume(input, TokenType.RIGHT_CURLY_BRACKET);
                    return parseRecord(List.of(), token);
                }
                final List<FunckyExpression> components = new ArrayList<>();
                while (true) {
                    components.add(parseComplexExpression(input,
                            Set.of(TokenType.COMMA, TokenType.RIGHT_CURLY_BRACKET)));
                    if (consume(input, Set.of(TokenType.COMMA, TokenType.RIGHT_CURLY_BRACKET)).type()
                            == TokenType.RIGHT_CURLY_BRACKET) {
                        return parseRecord(components, token);
                    }
                    consume(input, TokenType.SPACE);
                }
        }
        throw new IllegalStateException(String.format(UNEXPECTED_TOKEN, token));
    }

    private FunckyLiteral parseString(final String string, final Token token) {
        return new FunckyLiteral(engine, token.file(), token.line(), token.column(), new FunckyList(engine,
                new FunckyListType(engine, new FunckyLiteral(engine, token.file(), token.line(), token.column(),
                        FunckySimpleType.CHARACTER.apply(engine))), string.isEmpty() ? null
                        : new FunckyLiteral(engine, token.file(), token.line(), token.column(),
                                new FunckyCharacter(engine, string.charAt(0))),
                        string.isEmpty() ? null : parseString(string.substring(1), token)));
    }

    private URI parseUri(final Token string) {
        try {
            return new URI(string.stringValue());
        } catch (final URISyntaxException e) {
            throw new SneakyCompilationException(new InvalidUriException(string));
        }
    }

    private FunckyLiteral parseList(final List<FunckyExpression> elements, final Token leftSquareBracket) {
            final FunckyExpression head = elements.isEmpty() ? null : elements.getFirst();
            final FunckyLiteral tail = elements.isEmpty() ? null
                    : parseList(elements.subList(1, elements.size()), leftSquareBracket);
            return new FunckyLiteral(engine, leftSquareBracket.file(), leftSquareBracket.line(),
                    leftSquareBracket.column(), new FunckyList(engine, new FunckyListType(engine,
                    new FunckyLiteral(engine, new FunckyTypeVariable(engine))), head, tail));
    }

    private FunckyLiteral parseRecord(final List<FunckyExpression> components, final Token leftCurlyBracket) {
        final List<FunckyType> types = new ArrayList<>();
        for (final FunckyExpression component : components) {
            types.add(component.getType());
        }
        final FunckyRecordType recordType = (FunckyRecordType) FunckyRecordType.RECORD(types.stream()
                .map(t -> (Function<FunckyEngine, FunckyType>) (e -> t))
                .toList().toArray(new Function[0])).apply(engine);
        return new FunckyLiteral(engine, leftCurlyBracket.file(), leftCurlyBracket.line(), leftCurlyBracket.column(),
                new FunckyRecord(engine, recordType, components)); // TODO cleanup
    }

    private Token consume(final Queue<Token> input, final Set<TokenType> expected) {
        final Token token = peek(input, expected);
        input.remove();
        return token;
    }

    private Token consume(final Queue<Token> input, final TokenType expected) {
        return consume(input, Set.of(expected));
    }

    private Token peek(final Queue<Token> input, final Set<TokenType> expected) {
        final Token token = Objects.requireNonNull(input.peek());
        if (expected.contains(token.type())) {
            return token;
        }
        throw new SneakyCompilationException(new UnexpectedTokenException(token, new TreeSet<>(expected)));
    }
}
