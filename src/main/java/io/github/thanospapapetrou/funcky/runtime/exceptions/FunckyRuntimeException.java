package io.github.thanospapapetrou.funcky.runtime.exceptions;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.script.ScriptException;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;

public class FunckyRuntimeException extends ScriptException {
    private static final String APPLICATION = "%n    in `%1$s` in %2$s at line %3$d at column %4$d";
    private static final String DEFINITION = "%n  in `%1$s` in %2$s at line %3$d at column %4$d";
    private static final String ERROR_FORMATTING_STACK = "Error formatting stack for literal `%1$s`";
    private final List<FunckyExpression> stack;

    FunckyRuntimeException(final String message) {
        this(message, new ArrayList<>());
    }

    private FunckyRuntimeException(final String message, final List<FunckyExpression> stack) {
        super(message, null, -1, -1);
        this.stack = stack;
    }

    public List<FunckyExpression> getStack() {
        return stack;
    }

    @Override
    public String getFileName() {
        return stack.isEmpty() ? null : stack.getFirst().getFile().toString();
    }

    @Override
    public int getLineNumber() {
        return stack.isEmpty() ? -1 : stack.getFirst().getLine();
    }

    @Override
    public int getColumnNumber() {
        return stack.isEmpty() ? -1 : stack.getFirst().getColumn();
    }

    @Override
    public String getMessage() {
        return super.getMessage() + stack.stream().map(this::formatStack).collect(Collectors.joining());
    }

    private String formatStack(final FunckyExpression expression) {
        return switch (expression) {
            case FunckyApplication application -> formatStack(application);
            case FunckyReference reference -> formatStack(reference);
            case FunckyLiteral literal ->
                    throw new IllegalStateException(String.format(ERROR_FORMATTING_STACK, literal));
        };
    }

    private String formatStack(final FunckyApplication application) {
        return String.format(APPLICATION, application, application.getFile(), application.getLine(),
                application.getColumn());
    }

    private String formatStack(final FunckyReference reference) {
            final FunckyExpression expression = reference.getEngine().getContext()
                    .getDefinitionExpression(reference.normalize());
            return String.format(DEFINITION, new FunckyDefinition(expression.getFile(), expression.getLine(),
                    reference.getName(), expression), expression.getFile(), expression.getLine(), 1);
    }
}
