package io.github.thanospapapetrou.funcky.compiler;

import java.io.Reader;
import java.net.URI;
import java.util.ArrayDeque;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import io.github.thanospapapetrou.funcky.compiler.ast.Application;
import io.github.thanospapapetrou.funcky.compiler.ast.Definition;
import io.github.thanospapapetrou.funcky.compiler.ast.Expression;
import io.github.thanospapapetrou.funcky.compiler.ast.Import;
import io.github.thanospapapetrou.funcky.compiler.ast.Literal;
import io.github.thanospapapetrou.funcky.compiler.ast.Reference;
import io.github.thanospapapetrou.funcky.compiler.ast.Script;
import io.github.thanospapapetrou.funcky.compiler.exceptions.NameAlreadyDefinedException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.PrefixAlreadyBoundException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.exceptions.UnboundPrefixException;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.parser.Parser;
import io.github.thanospapapetrou.funcky.compiler.preprocessor.Preprocessor;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.TokenType;
import io.github.thanospapapetrou.funcky.compiler.tokenizer.Tokenizer;

public class Compiler {
    private final Tokenizer tokenizer;
    private final Parser parser;
    private final Preprocessor preprocessor;
    private final URI base;

    public Compiler(final URI base) {
        this(new Tokenizer(), new Parser(), new Preprocessor(), base);
    }

    private Compiler(final Tokenizer tokenizer, final Parser parser, final Preprocessor preprocessor, final URI base) {
        this.tokenizer = tokenizer;
        this.parser = parser;
        this.preprocessor = preprocessor;
        this.base = base;
    }

    public Expression compile(final String expression) {
        return canonicalize(preprocessor.preprocess(parser.parse(
                tokenizer.tokenize(expression).stream().filter(token -> token.type() != TokenType.COMMENT)
                        .collect(Collectors.toCollection(ArrayDeque::new)))), Map.of());
    }

    public Script compile(final Reader script, final URI file) {
        return canonicalize(preprocessor.preprocess(parser.parse(
                tokenizer.tokenize(script, file).stream().filter(token -> token.type() != TokenType.COMMENT)
                        .collect(Collectors.toCollection(ArrayDeque::new)), file)));
    }

    private Script canonicalize(final Script script) {
        final Map<String, URI> imports = canonicalize(script.imports());
        final Script canonical = new Script(script.file());
        canonical.definitions().addAll(canonicalize(script.definitions(), imports));
        return canonical;
    }

    private Map<String, URI> canonicalize(final List<Import> imports) {
        return imports.stream().collect(Collectors.toMap(Import::prefix, inport -> canonicalize(inport, imports)));
    }

    private List<Definition> canonicalize(final List<Definition> definitions, final Map<String, URI> imports) {
        return definitions.stream().map(definition -> canonicalize(definition, definitions, imports)).toList();
    }

    private URI canonicalize(final Import inport, final List<Import> imports) {
        final Optional<Import> duplicate = imports.stream().filter(other -> other.line() < inport.line())
                .filter(other -> other.prefix().equals(inport.prefix())).findAny();
        if (duplicate.isPresent()) {
            throw new SneakyCompilationException(new PrefixAlreadyBoundException(inport, duplicate.get()));
        }
        return canonicalize(inport.file(), inport.namespace());
    }

    private Definition canonicalize(final Definition definition, final List<Definition> definitions,
            final Map<String, URI> imports) {
        final Optional<Definition> duplicate = definitions.stream().filter(other -> other.line() < definition.line())
                .filter(other -> other.name().equals(definition.name())).findAny();
        if (duplicate.isPresent()) {
            throw new SneakyCompilationException(new NameAlreadyDefinedException(definition, duplicate.get()));
        }
        return new Definition(definition.file(), definition.line(), definition.name(),
                canonicalize(definition.expression(), imports));
    }

    private Expression canonicalize(final Expression expression, final Map<String, URI> imports) {
        return switch (expression) {
            case Literal literal -> literal;
            case Reference reference -> canonicalize(reference, imports);
            case Application application -> new Application(canonicalize(application.getFunction(), imports),
                    canonicalize(application.getArgument(), imports));
        };
    }

    private Reference canonicalize(final Reference reference, final Map<String, URI> imports) {
        if (reference.getPrefix() != null) {
            if (!imports.containsKey(reference.getPrefix())) {
                throw new SneakyCompilationException(new UnboundPrefixException(reference));
            }
            return new Reference(reference.getFile(), reference.getLine(), reference.getColumn(),
                    imports.get(reference.getPrefix()), reference.getName());
        }
        return reference;
    }

    private URI canonicalize(final URI base, final URI namespace) { // TODO is this required? move stdin here?
        return (namespace.isAbsolute() ? namespace
                : (base.equals(Linker.STDIN) ? this.base : base).resolve(namespace)).normalize();
    }
}
