package io.github.thanospapapetrou.funcky.compiler.preprocessor;

import java.util.Map;

import io.github.thanospapapetrou.funcky.compiler.parser.Application;
import io.github.thanospapapetrou.funcky.compiler.parser.Definition;
import io.github.thanospapapetrou.funcky.compiler.parser.Expression;
import io.github.thanospapapetrou.funcky.compiler.parser.Reference;
import io.github.thanospapapetrou.funcky.compiler.parser.Script;

public class Preprocessor {
    private static final String COMBINATOR_I = "i";
    private static final String COMBINATOR_K = "k";
    private static final String COMBINATOR_S = "s";
    private static final String REFERENCE_ARGUMENT = "$";
    private static final String REFERENCE_FUNCTION = "$$";

    public Expression preprocess(final Expression expression) {
        final Map.Entry<Integer, Expression> function = isFunction(expression);
        if (function != null) {
            Expression transformed = function.getValue();
            for (int argument = function.getKey() - 1; argument >= 0; argument--) {
                transformed = transform(argument, transformed);
            }
            return transformed;
        } else if (expression instanceof Application) { // TODO instanceof with var everywhere
            //            return new Application(preprocess(((Application) expression).getFunction()),
            //                    preprocess(((Application) expression).getArgument()));
            return null;
        }
        return expression;
    }

    public Script preprocess(final Script script) {
        final Script preprocessed = new Script(script.file());
        preprocessed.imports().addAll(script.imports());
        script.definitions().stream()
                .map(definition -> new Definition(definition.file(), definition.line(),
                        definition.name(), preprocess(definition.expression())))
                .forEach(preprocessed.definitions()::add);
        return preprocessed;
    }

    private Expression transform(final int argument, final Expression expression) {
        if (isArgument(argument, expression)) {
            return i(expression);
        } else if (expression instanceof Application) {
            if ((!containsArgument(argument, ((Application) expression).getFunction()))
                    && isArgument(argument, ((Application) expression).getArgument())) {
                return ((Application) expression).getFunction();
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
        if ((expression instanceof Application)
                && (((Application) expression).getFunction() instanceof Application)
                && (((Application) ((Application) expression).getFunction()).getFunction() instanceof Reference)
                && (((Reference) ((Application) ((Application) expression).getFunction())
                .getFunction()).getNamespace() == null)
                && (((Reference) ((Application) ((Application) expression).getFunction())
                .getFunction()).getPrefix() == null)
                && (((Reference) ((Application) ((Application) expression).getFunction())
                .getFunction()).getName().equals(REFERENCE_FUNCTION))) {
            //            final int arguments = ((FunckyNumber) ((Literal) ((Application) ((Application) expression)
            //                    .getFunction()).getArgument()).getValue()).getValue().intValue();
            //            final Expression body = ((Application) expression).getArgument();
            //            return new AbstractMap.SimpleEntry<>(arguments, body);
            return null;
        }
        return null;
    }

    private boolean isArgument(final int argument, final Expression expression) {
        return false;
        //        return (expression instanceof Application)
        //                && (((Application) expression).getFunction() instanceof Reference)
        //                && (((Reference) ((Application) expression).getFunction()).getNamespace() == null)
        //                && (((Reference) ((Application) expression).getFunction()).getPrefix() == null)
        //                && ((Reference) ((Application) expression).getFunction()).getName().equals(
        //                REFERENCE_ARGUMENT)
        //                && (((Application) expression).getArgument() instanceof Literal)
        //                && (((Literal) ((Application) expression).getArgument()).getNumber() != null)
        //                && (((FunckyNumber) ((Application) expression).getArgument().eval())
        //                .getValue().intValue() == argument);
    }

    private boolean containsArgument(final int argument, final Expression expression) {
        return isArgument(argument, expression) || ((expression instanceof Application)
                && (containsArgument(argument, ((Application) expression).getFunction())
                || containsArgument(argument, ((Application) expression).getArgument())));
    }

    private Reference i(final Expression expression) {
        return null;
        //        return new Reference(expression.getFile(), expression.getLine(),
        //                expression.getColumn(), Linker.getNamespace(Combinators.class), null, COMBINATOR_I, null);
    }

    private Application k(final Expression expression) {
        return null;
        //        return new Application(new Reference(expression.getFile(), expression.getLine(),
        //                expression.getColumn(), Linker.getNamespace(Combinators.class), null, COMBINATOR_K),
        //                expression);
    }

    private Application s(final int argument, final Expression expression) {
        return null;
        //        return new Application(new Application(
        //                new Reference(expression.getFile(), expression.getLine(), expression.getColumn(),
        //                        Linker.getNamespace(Combinators.class), null, COMBINATOR_S),
        //                transform(argument, ((Application) expression).getFunction())),
        //                transform(argument, ((Application) expression).getArgument()));
    }
}
