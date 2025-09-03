package io.github.thanospapapetrou.funcky.compiler.preprocessor;

import java.util.AbstractMap;
import java.util.Map;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.prelude.Combinators;

public class Preprocessor {
    private static final String COMBINATOR_I = "i";
    private static final String COMBINATOR_K = "k";
    private static final String COMBINATOR_S = "s";
    private static final String REFERENCE_ARGUMENT = "$";
    private static final String REFERENCE_FUNCTION = "$$";

    public FunckyExpression preprocess(final FunckyExpression expression) {
        final Map.Entry<Integer, FunckyExpression> function = isFunction(expression);
        if (function != null) {
            FunckyExpression transformed = function.getValue();
            for (int argument = function.getKey() - 1; argument >= 0; argument--) {
                transformed = transform(argument, transformed);
            }
            return transformed;
        } else if (expression instanceof FunckyApplication) {
            return new FunckyApplication(preprocess(((FunckyApplication) expression).getFunction()),
                    preprocess(((FunckyApplication) expression).getArgument()));
        }
        return expression;
    }

    public FunckyScript preprocess(final FunckyScript script) {
        final FunckyScript preprocessed = new FunckyScript(script.getEngine(), script.getFile());
        preprocessed.getImports().addAll(script.getImports());
        script.getDefinitions().stream()
                .map(definition -> new FunckyDefinition(definition.getFile(), definition.getLine(),
                        definition.getName(), preprocess(definition.getExpression())))
                .forEach(preprocessed.getDefinitions()::add);
        return preprocessed;
    }

    private FunckyExpression transform(final int argument, final FunckyExpression expression) {
        if (isArgument(argument, expression)) {
            return i(expression);
        } else if (expression instanceof FunckyApplication) {
            if ((!containsArgument(argument, ((FunckyApplication) expression).getFunction()))
                    && isArgument(argument, ((FunckyApplication) expression).getArgument())) {
                return ((FunckyApplication) expression).getFunction();
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
        if ((expression instanceof FunckyApplication)
                && (((FunckyApplication) expression).getFunction() instanceof FunckyApplication)
                && (((FunckyApplication) ((FunckyApplication) expression).getFunction()).getFunction() instanceof FunckyReference)
                && (((FunckyReference) ((FunckyApplication) ((FunckyApplication) expression).getFunction())
                .getFunction()).getNamespace() == null)
                && (((FunckyReference) ((FunckyApplication) ((FunckyApplication) expression).getFunction())
                .getFunction()).getPrefix() == null)
                && (((FunckyReference) ((FunckyApplication) ((FunckyApplication) expression).getFunction())
                .getFunction()).getName().equals(REFERENCE_FUNCTION))) {
            final int arguments = ((FunckyNumber) ((FunckyLiteral) ((FunckyApplication) ((FunckyApplication) expression)
                    .getFunction()).getArgument()).getValue()).getValue().intValue();
            final FunckyExpression body = ((FunckyApplication) expression).getArgument();
            return new AbstractMap.SimpleEntry<>(arguments, body);
        }
        return null;
    }

    private boolean isArgument(final int argument, final FunckyExpression expression) {
        return (expression instanceof FunckyApplication)
                && (((FunckyApplication) expression).getFunction() instanceof FunckyReference)
                && (((FunckyReference) ((FunckyApplication) expression).getFunction()).getNamespace() == null)
                && (((FunckyReference) ((FunckyApplication) expression).getFunction()).getPrefix() == null)
                && ((FunckyReference) ((FunckyApplication) expression).getFunction()).getName().equals(
                REFERENCE_ARGUMENT)
                && (((FunckyApplication) expression).getArgument() instanceof FunckyLiteral)
                && (((FunckyLiteral) ((FunckyApplication) expression).getArgument()).getValue() instanceof FunckyNumber)
                && (((FunckyNumber) ((FunckyLiteral) ((FunckyApplication) expression).getArgument()).getValue())
                .getValue().intValue() == argument);
        // TODO validate
    }

    private boolean containsArgument(final int argument, final FunckyExpression expression) {
        return isArgument(argument, expression) || ((expression instanceof FunckyApplication)
                && (containsArgument(argument, ((FunckyApplication) expression).getFunction())
                || containsArgument(argument, ((FunckyApplication) expression).getArgument())));
    }

    private FunckyReference i(final FunckyExpression expression) {
        return new FunckyReference(expression.getEngine(), expression.getFile(), expression.getLine(),
                expression.getColumn(), Linker.getNamespace(Combinators.class), COMBINATOR_I);
    }

    private FunckyApplication k(final FunckyExpression expression) {
        return new FunckyApplication(new FunckyReference(expression.getEngine(), expression.getFile(),
                expression.getLine(), expression.getColumn(), Linker.getNamespace(Combinators.class), COMBINATOR_K),
                expression);
    }

    private FunckyApplication s(final int argument, final FunckyExpression expression) {
        return new FunckyApplication(new FunckyApplication(new FunckyReference(expression.getEngine(),
                expression.getFile(), expression.getLine(), expression.getColumn(),
                Linker.getNamespace(Combinators.class), COMBINATOR_S),
                transform(argument, ((FunckyApplication) expression).getFunction())),
                transform(argument, ((FunckyApplication) expression).getArgument()));
    }
}
