package com.github.thanospapapetrou.funcky.parser;

import com.github.thanospapapetrou.funcky.FunckyEngine;
import com.github.thanospapapetrou.funcky.parser.tokenizer.Token;
import com.github.thanospapapetrou.funcky.parser.tokenizer.TokenType;
import com.github.thanospapapetrou.funcky.parser.tokenizer.Tokenizer;

import java.io.IOException;
import java.io.Reader;
import java.net.URI;
import java.util.Arrays;
import java.util.function.Function;
import java.util.function.Predicate;

public class Parser {
    private static final String ILLEGAL_STATE =
            "Parser is in illegal state: token %1$s was expected but not handled";
    private static final Predicate<Token> NO_COMMENT =
            ((Predicate<Token>) ((Function<TokenType, Boolean>) TokenType.COMMENT::equals)
                    .compose(Token::getType)::apply).negate();
    private static final Predicate<Token> NO_WHITESPACE =
            ((Predicate<Token>) ((Function<TokenType, Boolean>) TokenType.WHITESPACE::equals)
                    .compose(Token::getType)::apply).negate();

    private final FunckyEngine engine;
    private final PushbackIterator<Token> tokens;

    public Parser(final FunckyEngine engine, final Reader reader, final URI script)
            throws IOException {
        this.engine = engine;
        tokens = new PushbackIterator<>(new Tokenizer(reader, script).tokenize().filter(NO_COMMENT)
                .filter(NO_WHITESPACE).iterator());
    }

    void parseScript() {
        // TODO
    }

    public Expression parseExpression() throws UnexpectedTokenException {
        final Expression expression = _parseExpression();
        parse(TokenType.EOL);
        parse(TokenType.EOF);
        return expression;
    }

    private Expression _parseExpression() throws UnexpectedTokenException {
        Expression expression = parseSimpleExpression();
        while (true) {
            final Token token = parse(TokenType.EOL, TokenType.IDENTIFIER,
                    TokenType.LEFT_PARENTHESIS, TokenType.NUMBER, TokenType.RIGHT_PARENTHESIS);
            switch (token.getType()) {
            case EOL:
            case RIGHT_PARENTHESIS:
                tokens.pushback(token);
                return expression;
            case IDENTIFIER:
            case LEFT_PARENTHESIS:
            case NUMBER:
                tokens.pushback(token);
                expression = new Application(engine, expression, parseSimpleExpression());
                break;
            default:
                throw new ParserIllegalStateException(token);
            }
        }
    }

    private Expression parseSimpleExpression() throws UnexpectedTokenException {
        final Token token =
                parse(TokenType.IDENTIFIER, TokenType.LEFT_PARENTHESIS, TokenType.NUMBER);
        switch (token.getType()) {
        case IDENTIFIER:
            return new Reference(engine, token.getValue());
        case LEFT_PARENTHESIS:
            final Expression expression = _parseExpression();
            parse(TokenType.RIGHT_PARENTHESIS);
            return expression;
        case NUMBER:
            return new Number(engine, Double.valueOf(token.getValue()));
        default:
            throw new ParserIllegalStateException(token);
        }
    }

    private Token parse(final TokenType... expected) throws UnexpectedTokenException {
        final Token token = tokens.next();
        return Arrays.stream(expected).anyMatch(token.getType()::equals) ? token
                : unexpected(token, expected);
    }

    private Token unexpected(final Token token, final TokenType... expected)
            throws UnexpectedTokenException {
        throw new UnexpectedTokenException(token, expected);
    }
}
// TODO
// * <script> ::= <definition> <new line> <script>
// * | ε
// * <definition> ::= <symbol> <whitespace> "=" <whitespace> <expression>
// * <expression> ::= <simpleExpression>
// * | <application>
// * <simpleExpression> ::= <nestedExpression>
// * | <reference>
// * | <literal>
// * <application> ::= <expression> <whitespace> <simpleExpression>
// * <nestedExpression> ::= "(" <expression> ")"
// * <reference> ::= <identifier>
// * <literal> ::= <number>
// * <new line> ::= "\n"
// * | "\r"
// * | "\r\n"
// * <identifier> ::= [\w&&\D]\w*
// * <number> ::= \-?\d(\.\d*)?
// * | \-?.\d*
