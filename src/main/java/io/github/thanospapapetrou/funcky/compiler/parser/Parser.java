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

import io.github.thanospapapetrou.funcky.compiler.ast.Application;
import io.github.thanospapapetrou.funcky.compiler.ast.Definition;
import io.github.thanospapapetrou.funcky.compiler.ast.Expression;
import io.github.thanospapapetrou.funcky.compiler.ast.Import;
import io.github.thanospapapetrou.funcky.compiler.ast.Literal;
import io.github.thanospapapetrou.funcky.compiler.ast.Reference;
import io.github.thanospapapetrou.funcky.compiler.ast.Script;
import io.github.thanospapapetrou.funcky.compiler.exceptions.InvalidUriException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnexpectedTokenException;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Token;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.TokenType;

public class Parser {
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
    private static final String FORMAT_IMPORT = "%1$s `%2$s` %3$s %4$d 1";
    private static final String FORMAT_DEFINITION = "%1$s `%2$s` %3$s %4$d 1";
    private static final String FORMAT_EXPRESSION = "%1$s `%2$s` %3$s %4$d %5$d";
    private static final String FORMAT_SCRIPT = "Script %1$s 1 1";
    private static final String INDENTATION = "  ";
    private static final Logger LOGGER = Logger.getLogger(Parser.class.getName());
    private static final String UNEXPECTED_TOKEN = "Unexpected token `%1$s`";

    private static Set<TokenType> union(final Set<TokenType> a, final Set<TokenType> b) {
        final Set<TokenType> result = new HashSet<>();
        result.addAll(a);
        result.addAll(b);
        return result;
    }

    private static void log(final Expression expression, final int indentation) {
        if (expression != null) {
            LOGGER.finer(INDENTATION.repeat(indentation) + String.format(FORMAT_EXPRESSION,
                    expression.getClass().getSimpleName(), expression, expression.getFile(), expression.getLine(),
                    expression.getColumn()));
            if (expression instanceof Literal literal) {
                if (literal.getList() != null) {
                    literal.getList()
                            .forEach(element -> log(element, indentation + 1));
                } else if (literal.getRecord() != null) {
                    literal.getRecord()
                            .forEach(component -> log(component, indentation + 1));
                }
            } else if (expression instanceof Application application) {
                log(application.getFunction(), indentation + 1);
                log(application.getArgument(), indentation + 1);
            }
        }
    }

    private static void log(final Script script) {
        LOGGER.finer(String.format(FORMAT_SCRIPT, script));
        script.imports().stream()
                .map(inport -> INDENTATION + String.format(FORMAT_IMPORT, Import.class.getSimpleName(), inport,
                        inport.file(), inport.line()))
                .forEach(LOGGER::finer);
        for (final Definition definition : script.definitions()) {
            LOGGER.finer(INDENTATION + String.format(FORMAT_DEFINITION, Definition.class.getSimpleName(), definition,
                    definition.file(), definition.line()));
            log(definition.expression(), 2);
        }
    }

    public Expression parse(final Queue<Token> input) {
        final Expression expression = (peek(input, union(FIRST, Set.of(TokenType.EOL))).type() == TokenType.EOL) ? null
                : parseComplexExpression(input, Set.of(TokenType.EOL));
        consume(input, TokenType.EOL);
        consume(input, TokenType.EOF);
        log(expression, 0);
        return expression;
    }

    public Script parse(final Queue<Token> input, final URI file) {
        final Script script = new Script(file);
        while (true) {
            final Token token = consume(input, Set.of(TokenType.SYMBOL, TokenType.EOL, TokenType.EOF));
            switch (token.type()) {
                case SYMBOL:
                    if (consume(input, Set.of(TokenType.COLON, TokenType.SPACE)).type() == TokenType.COLON) {
                        consume(input, TokenType.SPACE);
                        final URI namespace = parseUri(consume(input, TokenType.STRING));
                        consume(input, TokenType.EOL);
                        script.imports().add(new Import(token.file(), token.line(), token.value(), namespace));
                    } else {
                        consume(input, TokenType.EQUAL);
                        consume(input, TokenType.SPACE);
                        final Expression expression = parseComplexExpression(input, Set.of(TokenType.EOL));
                        consume(input, TokenType.EOL);
                        script.definitions().add(new Definition(token.file(), token.line(), token.value(), expression));
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

    private Expression parseComplexExpression(final Queue<Token> input, final Set<TokenType> follow) {
        Expression expression = parseSimpleExpression(input, follow);
        while (true) {
            if (peek(input, union(Set.of(TokenType.SPACE), follow)).type() == TokenType.SPACE) {
                consume(input, TokenType.SPACE);
                expression = new Application(expression, parseSimpleExpression(input, follow));
            } else {
                return expression;
            }
        }
    }

    private Expression parseSimpleExpression(final Queue<Token> input, final Set<TokenType> follow) {
        final Token token = consume(input, FIRST);
        switch (token.type()) {
            case BINARY_NUMBER:
            case OCTAL_NUMBER:
            case HEXADECIMAL_NUMBER:
                return Literal.number(token.file(), token.line(), token.column(),
                        new BigDecimal(new BigInteger(token.signedValue(), token.type().getRadix().getRadix())));
            case DECIMAL_NUMBER:
                return Literal.number(token.file(), token.line(), token.column(), new BigDecimal(token.value()));
            case CHARACTER:
                return Literal.character(token.file(), token.line(), token.column(), token.stringValue().charAt(0));
            case OCTAL_CHARACTER:
            case HEXADECIMAL_CHARACTER:
                return Literal.character(token.file(), token.line(), token.column(),
                        (char) Integer.parseInt(token.unsignedValue(), token.type().getRadix().getRadix()));
            case STRING:
                if (peek(input, union(Set.of(TokenType.PERIOD, TokenType.SPACE), follow)).type() == TokenType.PERIOD) {
                    consume(input, TokenType.PERIOD);
                    return new Reference(token.file(), token.line(), token.column(), parseUri(token), consume(input,
                            TokenType.SYMBOL).value());
                }
                return Literal.string(token.file(), token.line(), token.column(), token.stringValue());
            case SYMBOL:
                if (peek(input, union(Set.of(TokenType.PERIOD, TokenType.SPACE), follow)).type() == TokenType.PERIOD) {
                    consume(input, TokenType.PERIOD);
                    return new Reference(token.file(), token.line(), token.column(), token.value(), consume(input,
                            TokenType.SYMBOL).value());
                }
                return new Reference(token.file(), token.line(), token.column(), token.value());
            case LEFT_PARENTHESIS:
                final Expression expression = parseComplexExpression(input, Set.of(TokenType.RIGHT_PARENTHESIS));
                consume(input, TokenType.RIGHT_PARENTHESIS);
                return expression;
            case LEFT_SQUARE_BRACKET:
                if (peek(input, union(FIRST, Set.of(TokenType.RIGHT_SQUARE_BRACKET))).type()
                        == TokenType.RIGHT_SQUARE_BRACKET) {
                    consume(input, TokenType.RIGHT_SQUARE_BRACKET);
                    return Literal.list(token.file(), token.line(), token.column(), List.of());
                }
                final List<Expression> list = new ArrayList<>();
                while (true) {
                    list.add(parseComplexExpression(input, Set.of(TokenType.COMMA,
                            TokenType.RIGHT_SQUARE_BRACKET)));
                    if (consume(input, Set.of(TokenType.COMMA, TokenType.RIGHT_SQUARE_BRACKET)).type()
                            == TokenType.RIGHT_SQUARE_BRACKET) {
                        return Literal.list(token.file(), token.line(), token.column(), list);
                    }
                    consume(input, TokenType.SPACE);
                }
            case LEFT_CURLY_BRACKET:
                if (peek(input, union(FIRST, Set.of(TokenType.RIGHT_CURLY_BRACKET))).type()
                        == TokenType.RIGHT_CURLY_BRACKET) {
                    consume(input, TokenType.RIGHT_CURLY_BRACKET);
                    return Literal.record(token.file(), token.line(), token.column(), List.of());
                }
                final List<Expression> record = new ArrayList<>();
                while (true) {
                    record.add(parseComplexExpression(input, Set.of(TokenType.COMMA,
                            TokenType.RIGHT_CURLY_BRACKET)));
                    if (consume(input, Set.of(TokenType.COMMA, TokenType.RIGHT_CURLY_BRACKET)).type()
                            == TokenType.RIGHT_CURLY_BRACKET) {
                        return Literal.record(token.file(), token.line(), token.column(), record);
                    }
                    consume(input, TokenType.SPACE);
                }
        }
        throw new IllegalStateException(String.format(UNEXPECTED_TOKEN, token));
    }

    private URI parseUri(final Token string) {
        try {
            return new URI(string.stringValue());
        } catch (final URISyntaxException e) {
            throw new SneakyCompilationException(new InvalidUriException(string));
        }
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
