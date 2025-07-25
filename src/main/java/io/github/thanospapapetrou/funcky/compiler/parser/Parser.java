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
import java.util.logging.Logger;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyImport;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.parser.exceptions.InvalidListLiteralException;
import io.github.thanospapapetrou.funcky.compiler.parser.exceptions.InvalidUriException;
import io.github.thanospapapetrou.funcky.compiler.parser.exceptions.UnexpectedTokenException;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Token;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.TokenType;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public class Parser {
    private static final String FINER_DEFINITION = "%1$sDefinition `%2$s` %3$s %4$d 1";
    private static final String FINER_EXPRESSION = "%1$s%2$s `%3$s` %4$s %5$d %6$d";
    private static final String FINER_IMPORT = "%1$sImport `%2$s` %3$s %4$d 1";
    private static final String FINER_SCRIPT = "Script %1$s 1 1";
    private static final Set<TokenType> FIRST = Set.of(TokenType.BINARY_NUMBER, TokenType.OCTAL_NUMBER,
            TokenType.DECIMAL_NUMBER, TokenType.HEXADECIMAL_NUMBER, TokenType.CHARACTER, TokenType.OCTAL_CHARACTER,
            TokenType.HEXADECIMAL_CHARACTER, TokenType.STRING, TokenType.SYMBOL, TokenType.LEFT_PARENTHESIS,
            TokenType.LEFT_SQUARE_BRACKET, TokenType.LEFT_CURLY_BRACKET);
    private static final String INDENTATION = "  ";
    private static final Logger LOGGER = Logger.getLogger(Parser.class.getName());
    private static final String TYPE_VARIABLE = "$_";

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
                    String.format(FINER_EXPRESSION, INDENTATION.repeat(indentation),
                            expression.getClass().getSimpleName(),
                            expression, expression.getFile(), expression.getLine(), expression.getColumn()));
            if (expression instanceof FunckyApplication) {
                log(((FunckyApplication) expression).getFunction(), indentation + 1);
                log(((FunckyApplication) expression).getArgument(), indentation + 1);
            }
        }
        if (indentation == 0) {
            LOGGER.finer("");
        }
    }

    private static void log(final FunckyScript script) {
        LOGGER.finer(String.format(FINER_SCRIPT, script.getFile()));
        for (final FunckyImport imp : script.getImports()) {
            LOGGER.finer(String.format(FINER_IMPORT, INDENTATION, imp, imp.getFile(), imp.getLine()));
        }
        for (final FunckyDefinition definition : script.getDefinitions()) {
            LOGGER.finer(String.format(FINER_DEFINITION, INDENTATION, definition, definition.getFile(),
                    definition.getLine()));
            log(definition.getExpression(), 2);
        }
        LOGGER.finer("");
    }

    public Parser(final FunckyEngine engine) {
        this.engine = engine;
    }

    public FunckyExpression parse(final Queue<Token> input) throws CompilationException {
        final FunckyExpression expression =
                (peek(input, union(FIRST, Set.of(TokenType.EOL))).getType() == TokenType.EOL) ? null
                        : parseComplexExpression(input, Set.of(TokenType.EOL));
        consume(input, TokenType.EOL);
        consume(input, TokenType.EOF);
        log(expression, 0);
        return expression;
    }

    public FunckyScript parse(final Queue<Token> input, final URI file) throws CompilationException {
        final FunckyScript script = new FunckyScript(engine, file);
        while (true) {
            final Token token = consume(input, Set.of(TokenType.SYMBOL, TokenType.EOL, TokenType.EOF));
            switch (token.getType()) {
                case SYMBOL:
                    if (consume(input, Set.of(TokenType.COLON, TokenType.SPACE)).getType() == TokenType.COLON) {
                        consume(input, TokenType.SPACE);
                        final URI namespace = parseUri(consume(input, TokenType.STRING));
                        consume(input, TokenType.EOL);
                        script.getImports().add(new FunckyImport(token.getFile(), token.getLine(), token.getValue(),
                                namespace));
                    } else {
                        consume(input, TokenType.EQUAL);
                        consume(input, TokenType.SPACE);
                        final FunckyExpression expression = parseComplexExpression(input, Set.of(TokenType.EOL));
                        consume(input, TokenType.EOL);
                        script.getDefinitions().add(new FunckyDefinition(token.getFile(), token.getLine(),
                                token.getValue(), expression));
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

    private FunckyExpression parseComplexExpression(final Queue<Token> input, final Set<TokenType> follow)
            throws CompilationException {
        FunckyExpression expression = parseSimpleExpression(input, follow);
        while (true) {
            if (peek(input, union(Set.of(TokenType.SPACE), follow)).getType() == TokenType.SPACE) {
                consume(input, TokenType.SPACE);
                expression = new FunckyApplication(expression, parseSimpleExpression(input, follow));
            } else {
                return expression;
            }
        }
    }

    private FunckyExpression parseSimpleExpression(final Queue<Token> input, final Set<TokenType> follow)
            throws CompilationException {
        final Token token = consume(input, FIRST);
        switch (token.getType()) {
            case BINARY_NUMBER:
            case OCTAL_NUMBER:
            case HEXADECIMAL_NUMBER:
                return new FunckyLiteral(engine, token.getFile(), token.getLine(), token.getColumn(),
                        new FunckyNumber(new BigDecimal(new BigInteger(token.getSignedValue(),
                                token.getType().getRadix().getRadix()))));
            case DECIMAL_NUMBER:
                return new FunckyLiteral(engine, token.getFile(), token.getLine(), token.getColumn(),
                        new FunckyNumber(new BigDecimal(token.getValue())));
            case CHARACTER:
                return new FunckyLiteral(engine, token.getFile(), token.getLine(), token.getColumn(),
                        new FunckyCharacter(token.getStringValue().charAt(0)));
            case OCTAL_CHARACTER:
            case HEXADECIMAL_CHARACTER:
                return new FunckyLiteral(engine, token.getFile(), token.getLine(), token.getColumn(),
                        new FunckyCharacter((char) Integer.parseInt(token.getUnsignedValue(),
                                token.getType().getRadix().getRadix())));
            case STRING:
                if (peek(input, union(Set.of(TokenType.PERIOD, TokenType.SPACE), follow)).getType()
                        == TokenType.PERIOD) {
                    consume(input, TokenType.PERIOD);
                    return new FunckyReference(engine, token.getFile(), token.getLine(), token.getColumn(),
                            parseUri(token), consume(input, TokenType.SYMBOL).getValue());
                }
                return parseString(token.getStringValue(), token);
            case SYMBOL:
                if (peek(input, union(Set.of(TokenType.PERIOD, TokenType.SPACE), follow)).getType()
                        == TokenType.PERIOD) {
                    consume(input, TokenType.PERIOD);
                    return new FunckyReference(engine, token.getFile(), token.getLine(), token.getColumn(),
                            token.getValue(),
                            consume(input, TokenType.SYMBOL).getValue());
                }
                return token.getValue().equals(TYPE_VARIABLE)
                        ? new FunckyLiteral(engine, token.getFile(), token.getLine(), token.getColumn(),
                        new FunckyTypeVariable())
                        : new FunckyReference(engine, token.getFile(), token.getLine(), token.getColumn(),
                                token.getValue());
            case LEFT_PARENTHESIS:
                final FunckyExpression expression = parseComplexExpression(input, Set.of(TokenType.RIGHT_PARENTHESIS));
                consume(input, TokenType.RIGHT_PARENTHESIS);
                return expression;
            case LEFT_SQUARE_BRACKET:
                if (peek(input, union(FIRST, Set.of(TokenType.RIGHT_SQUARE_BRACKET))).getType()
                        == TokenType.RIGHT_SQUARE_BRACKET) {
                    consume(input, TokenType.RIGHT_SQUARE_BRACKET);
                    return parseList(List.of(), token);
                }
                final List<FunckyExpression> elements = new ArrayList<>();
                while (true) {
                    elements.add(parseComplexExpression(input,
                            Set.of(TokenType.COMMA, TokenType.RIGHT_SQUARE_BRACKET)));
                    if (consume(input, Set.of(TokenType.COMMA, TokenType.RIGHT_SQUARE_BRACKET)).getType()
                            == TokenType.RIGHT_SQUARE_BRACKET) {
                        return parseList(elements, token);
                    }
                    consume(input, TokenType.SPACE);
                }
            case LEFT_CURLY_BRACKET:
                if (peek(input, union(FIRST, Set.of(TokenType.RIGHT_CURLY_BRACKET))).getType()
                        == TokenType.RIGHT_CURLY_BRACKET) {
                    consume(input, TokenType.RIGHT_CURLY_BRACKET);
                    return parseRecord(List.of(), token);
                }
                final List<FunckyExpression> components = new ArrayList<>();
                while (true) {
                    components.add(parseComplexExpression(input,
                            Set.of(TokenType.COMMA, TokenType.RIGHT_CURLY_BRACKET)));
                    if (consume(input, Set.of(TokenType.COMMA, TokenType.RIGHT_CURLY_BRACKET)).getType()
                            == TokenType.RIGHT_CURLY_BRACKET) {
                        return parseRecord(components, token);
                    }
                    consume(input, TokenType.SPACE);
                }
        }
        return null; // TODO illegal state exception
    }

    private FunckyLiteral parseString(final String string, final Token token) {
        return new FunckyLiteral(engine, token.getFile(), token.getLine(), token.getColumn(),
                new FunckyList(new FunckyListType(new FunckyLiteral(engine, token.getFile(), token.getLine(),
                        token.getColumn(), FunckySimpleType.CHARACTER)), string.isEmpty() ? null
                        : new FunckyLiteral(engine, token.getFile(), token.getLine(), token.getColumn(),
                                new FunckyCharacter(string.charAt(0))),
                        string.isEmpty() ? null : parseString(string.substring(1), token)));
    }

    private URI parseUri(final Token string) throws InvalidUriException {
        try {
            return new URI(string.getStringValue());
        } catch (final URISyntaxException e) {
            throw new InvalidUriException(string);
        }
    }

    private FunckyLiteral parseList(final List<FunckyExpression> elements, final Token leftSquareBracket)
            throws CompilationException {
        try {
            final FunckyExpression head = elements.isEmpty() ? null : elements.get(0);
            final FunckyLiteral tail = elements.isEmpty() ? null
                    : parseList(elements.subList(1, elements.size()), leftSquareBracket);
            final FunckyType headType = (head == null) ? new FunckyTypeVariable()
                    : head.getType();
            final FunckyListType tailType = (tail == null) ? new FunckyListType(new FunckyTypeVariable())
                    : ((FunckyListType) tail.getType());
            final FunckyListType listType = (FunckyListType) new FunckyListType(headType).unify(tailType);
            if (listType == null) {
                throw new InvalidListLiteralException(head, tail, headType, tailType);
            }
            return new FunckyLiteral(engine, leftSquareBracket.getFile(), leftSquareBracket.getLine(),
                    leftSquareBracket.getColumn(), new FunckyList(listType, head, tail));
        } catch (final FunckyRuntimeException e) {
            throw new CompilationException(e);
        }
    }

    private FunckyLiteral parseRecord(final List<FunckyExpression> components, final Token leftCurlyBracket)
            throws CompilationException {
        final List<FunckyType> types = new ArrayList<>();
        for (final FunckyExpression component : components) {
            types.add(component.getType());
        }
        return new FunckyLiteral(engine, leftCurlyBracket.getFile(), leftCurlyBracket.getLine(),
                leftCurlyBracket.getColumn(),
                new FunckyRecord(new FunckyRecordType(engine.getConverter().convert(types)), components));
    }

    private Token consume(final Queue<Token> input, final Set<TokenType> expected) throws UnexpectedTokenException {
        final Token token = peek(input, expected);
        input.remove();
        return token;
    }

    private Token consume(final Queue<Token> input, final TokenType expected) throws UnexpectedTokenException {
        return consume(input, Set.of(expected));
    }

    private Token peek(final Queue<Token> input, final Set<TokenType> expected) throws UnexpectedTokenException {
        final Token token = Objects.requireNonNull(input.peek());
        if (expected.contains(token.getType())) {
            return token;
        }
        throw new UnexpectedTokenException(token, new TreeSet<>(expected));
    }
}
