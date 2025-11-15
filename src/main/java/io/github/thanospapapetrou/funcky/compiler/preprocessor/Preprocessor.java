package io.github.thanospapapetrou.funcky.compiler.preprocessor;

import java.math.BigDecimal;
import java.util.AbstractMap;
import java.util.Map;

import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.compiler.ast.Application;
import io.github.thanospapapetrou.funcky.compiler.ast.Definition;
import io.github.thanospapapetrou.funcky.compiler.ast.Expression;
import io.github.thanospapapetrou.funcky.compiler.ast.Literal;
import io.github.thanospapapetrou.funcky.compiler.ast.Reference;
import io.github.thanospapapetrou.funcky.compiler.ast.Script;
import io.github.thanospapapetrou.funcky.runtime.prelude.Combinators;

public class Preprocessor {
    private static final String COMBINATOR_I = "i";
    private static final String COMBINATOR_K = "k";
    private static final String COMBINATOR_S = "s";
    private static final String REFERENCE_ARGUMENT = "$";
    private static final String REFERENCE_FUNCTION = "$$";

    public Expression preprocess(final Expression expression) {
        final Map.Entry<Integer, Expression> function = isFunction(expression);
        if (function != null) {
            // TODO verify that no argument references are out of range
            // TODO arguments like $0 and not ($ 0)
            Expression transformed = function.getValue();
            for (int argument = function.getKey() - 1; argument >= 0; argument--) {
                transformed = transform(argument, transformed);
            }
            return transformed;
        } else if (expression instanceof Application application) {
            return new Application(preprocess(application.getFunction()), preprocess(application.getArgument()));
        }
        return expression;
    }

    public Script preprocess(final Script script) {
        final Script preprocessed = new Script(script.file());
        preprocessed.imports().addAll(script.imports());
        script.definitions().stream()
                .map(definition -> new Definition(definition.file(), definition.line(), definition.name(),
                        preprocess(definition.expression())))
                .forEach(preprocessed.definitions()::add);
        return preprocessed;
    }

    private Expression transform(final int argument, final Expression expression) {
        if (isArgument(argument, expression)) {
            return i(expression);
        } else if (expression instanceof Application application) {
            if ((!containsArgument(argument, application.getFunction()))
                    && isArgument(argument, application.getArgument())) {
                return application.getFunction();
            }
            for (int otherArgument = 0; otherArgument < argument; otherArgument++) {
                if (isArgument(otherArgument, expression)) {
                    return k(expression);
                }
            }
            return s(argument, expression);
        }
        return k(expression);
    }

    private Map.Entry<Integer, Expression> isFunction(final Expression expression) {
        if ((expression instanceof Application application)
                && (application.getFunction() instanceof Application otherApplication)
                && (otherApplication.getFunction() instanceof Reference reference)
                && (reference.getNamespace() == null)
                && (reference.getPrefix() == null)
                && (reference.getName().equals(REFERENCE_FUNCTION))) {
            if ((otherApplication.getArgument() instanceof Literal literal)
                    && (literal.getNumber() != null)
                    && (literal.getNumber().compareTo(new BigDecimal(literal.getNumber().intValue())) == 0)
                    && (literal.getNumber().intValue() > 0)) {
                return new AbstractMap.SimpleEntry<>(literal.getNumber().intValue(), application.getArgument());
            } else {
                throw new RuntimeException("Invalid function"); // TODO
            }
        }
        return null;
    }

    private boolean isArgument(final int argument, final Expression expression) {
        if ((expression instanceof Application application)
                && (application.getFunction() instanceof Reference reference)
                && (reference.getNamespace() == null)
                && (reference.getPrefix() == null)
                && reference.getName().equals(REFERENCE_ARGUMENT)) {
            if ((application.getArgument() instanceof Literal literal)
                    && (literal.getNumber() != null)
                    && (literal.getNumber().compareTo(new BigDecimal(literal.getNumber().intValue())) == 0)
                    && (literal.getNumber().intValue() >= 0)) {
                return literal.getNumber().intValue() == argument;
            } else {
                throw new RuntimeException("Invalid argument"); // TODO
            }
        }
        return false;
    }

    private boolean containsArgument(final int argument, final Expression expression) {
        return isArgument(argument, expression)
                || ((expression instanceof Application application)
                && (containsArgument(argument, application.getFunction())
                || containsArgument(argument, application.getArgument())));
    }

    private Reference i(final Expression expression) {
        return new Reference(expression.getFile(), expression.getLine(), expression.getColumn(),
                Linker.getNamespace(Combinators.class), COMBINATOR_I);
    }

    private Application k(final Expression expression) {
        return new Application(new Reference(expression.getFile(), expression.getLine(), expression.getColumn(),
                Linker.getNamespace(Combinators.class), COMBINATOR_K), expression);
    }

    private Application s(final int argument, final Expression expression) {
        return new Application(new Application(new Reference(expression.getFile(), expression.getLine(),
                expression.getColumn(), Linker.getNamespace(Combinators.class), COMBINATOR_S),
                transform(argument, ((Application) expression).getFunction())),
                transform(argument, ((Application) expression).getArgument()));
    }
}
