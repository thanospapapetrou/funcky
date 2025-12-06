package io.github.thanospapapetrou.funcky.compiler.preprocessor;

import java.math.BigDecimal;
import java.util.AbstractMap;
import java.util.Map;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.SneakyCompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.preprocessor.exceptions.InvalidArgumentCountException;
import io.github.thanospapapetrou.funcky.compiler.preprocessor.exceptions.InvalidArgumentIndexException;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.prelude.Combinators;

public class Preprocessor {
    private static final String COMBINATOR_I = "i"; // TODO replace with references
    private static final String COMBINATOR_K = "k";
    private static final String COMBINATOR_S = "s";
    private static final String REFERENCE_ARGUMENT = "$";
    private static final String REFERENCE_FUNCTION = "$$";

    private final FunckyEngine engine;

    public Preprocessor(final FunckyEngine engine) {
        this.engine = engine;
    }

    public FunckyExpression preprocess(final FunckyExpression expression) {
        final Map.Entry<Integer, FunckyExpression> function = isFunction(expression);
        if (function != null) {
            FunckyExpression transformed = function.getValue();
            for (int argument = function.getKey() - 1; argument >= 0; argument--) {
                transformed = transform(argument, transformed);
            }
            return transformed;
        } else if (expression instanceof FunckyApplication application) {
            return new FunckyApplication(preprocess(application.getFunction()), preprocess(application.getArgument()));
        }
        return expression;
    }

    public FunckyScript preprocess(final FunckyScript script) {
        final FunckyScript preprocessed = new FunckyScript(script.getEngine(), script.getFile());
        preprocessed.getImports().addAll(script.getImports());
        script.getDefinitions().stream()
                .map(definition -> new FunckyDefinition(definition.file(), definition.line(), definition.name(),
                        preprocess(definition.expression())))
                .forEach(preprocessed.getDefinitions()::add);
        return preprocessed;
    }

    private FunckyExpression transform(final int argument, final FunckyExpression expression) {
        // TODO unreduce or expand?
        if (isArgument(argument, expression)) {
            // TODO identify stray arguments (check for $ outside $$ and ($ N)
            // where N >= arguments
            return i(expression);
        } else if (expression instanceof FunckyApplication application) {
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

    private Map.Entry<Integer, FunckyExpression> isFunction(final FunckyExpression expression) {
        if ((expression instanceof FunckyApplication application)
                && (application.getFunction() instanceof FunckyApplication otherApplication)
                && (otherApplication.getFunction() instanceof FunckyReference reference)
                && (reference.getNamespace() == null) && (reference.getPrefix() == null)
                && (reference.getName().equals(REFERENCE_FUNCTION))) {
            if ((otherApplication.getArgument() instanceof FunckyLiteral literal)
                    && (literal.getValue() instanceof FunckyNumber number)
                    && (number.getValue().compareTo(new BigDecimal(number.getValue().intValue())) == 0)
                    && (number.getValue().intValue() > 0)) {
                return new AbstractMap.SimpleEntry<>(number.getValue().intValue(), application.getArgument());
            } else {
                throw new SneakyCompilationException(new InvalidArgumentCountException(otherApplication.getArgument()));
            }
        }
        return null;
    }

    private boolean isArgument(final int argument, final FunckyExpression expression) {
        if ((expression instanceof FunckyApplication application)
                && (application.getFunction() instanceof FunckyReference reference)
                && (reference.getNamespace() == null) && (reference.getPrefix() == null)
                && reference.getName().equals(REFERENCE_ARGUMENT)) {
            if ((application.getArgument() instanceof FunckyLiteral literal)
                    && (literal.getValue() instanceof FunckyNumber number)
                    && (number.getValue().compareTo(new BigDecimal(number.getValue().intValue())) == 0)
                    && (number.getValue().intValue() >= 0)) {
                return number.getValue().intValue() == argument;
            } else {
                throw new SneakyCompilationException(new InvalidArgumentIndexException(application.getArgument()));
            }
        }
        return false;
    }

    private boolean containsArgument(final int argument, final FunckyExpression expression) {
        return isArgument(argument, expression) || ((expression instanceof FunckyApplication application)
                && (containsArgument(argument, application.getFunction())
                || containsArgument(argument, application.getArgument())));
    }

    private FunckyReference i(final FunckyExpression expression) {
        return new FunckyReference(expression.getEngine(), expression.getFile(), expression.getLine(),
                expression.getColumn(), engine.getLinker().getNamespace(Combinators.class), COMBINATOR_I);
    }

    private FunckyApplication k(final FunckyExpression expression) {
        return new FunckyApplication(new FunckyReference(expression.getEngine(), expression.getFile(),
                expression.getLine(), expression.getColumn(), engine.getLinker().getNamespace(Combinators.class),
                COMBINATOR_K), expression);
    }

    private FunckyApplication s(final int argument, final FunckyExpression expression) {
        return new FunckyApplication(new FunckyApplication(new FunckyReference(expression.getEngine(),
                expression.getFile(), expression.getLine(), expression.getColumn(),
                engine.getLinker().getNamespace(Combinators.class), COMBINATOR_S),
                transform(argument, ((FunckyApplication) expression).getFunction())),
                transform(argument, ((FunckyApplication) expression).getArgument()));
    }
}
