package io.github.thanospapapetrou.funcky.runtime.exceptions;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.script.ScriptException;

import io.github.thanospapapetrou.funcky.compiler.CompilationException;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyApplication;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyDefinition;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyReference;

public class FunckyRuntimeException extends ScriptException {
    private static final String APPLICATION = "%n    in `%1$s` in %2$s at line %3$d at column %4$d";
    private static final String DEFINITION = "%n  in `%1$s` in %2$s at line %3$d at column %4$d";
    private static final String RUNTIME_COMPILATION_ERROR = "Compilation error detected at runtime: %1$s";
    private final List<FunckyExpression> stackTrace;

    public FunckyRuntimeException(final String message) {
        this(message, null, -1, -1, new ArrayList<>());
    }

    public FunckyRuntimeException(final CompilationException e) { // TODO remove
        this(String.format(RUNTIME_COMPILATION_ERROR, e.getMessage()), e.getFileName(), e.getLineNumber(),
                e.getColumnNumber(), new ArrayList<>());
    }

    private FunckyRuntimeException(final String message, final String file, final int line, final int column,
            final List<FunckyExpression> stackTrace) {
        super(message, file, line, column);
        this.stackTrace = stackTrace;
    }

    public void addStackTrace(final FunckyApplication application) {
        stackTrace.add(application);
    }

    public void addStackTrace(final FunckyReference reference) {
        stackTrace.add(reference);
    }

    @Override
    public String getFileName() {
        return stackTrace.isEmpty() ? null : stackTrace.get(0).getFile().toString();
    }

    @Override
    public int getLineNumber() {
        return stackTrace.isEmpty() ? -1 : stackTrace.get(0).getLine();
    }

    @Override
    public int getColumnNumber() {
        return stackTrace.isEmpty() ? -1 : stackTrace.get(0).getColumn();
    }

    @Override
    public String getMessage() {
        return super.getMessage() + stackTrace.stream().map(this::formatStackTrace).collect(Collectors.joining());
    }

    private String formatStackTrace(final FunckyExpression expression) {
        if (expression instanceof FunckyApplication) {
            return formatStackTrace((FunckyApplication) expression);
        } else if (expression instanceof FunckyReference) {
            return formatStackTrace((FunckyReference) expression);
        } else {
            return null; // TODO IllegalStateException?
        }
    }

    private String formatStackTrace(final FunckyApplication application) {
        return String.format(APPLICATION, application, application.getFile(), application.getLine(),
                application.getColumn());
    }

    private String formatStackTrace(final FunckyReference reference) {
        try {
            final FunckyExpression expression = reference.getEngine().getContext()
                    .getDefinitionExpression(reference.resolveNamespace(reference.getEngine().getContext()),
                            reference.getName());
            return String.format(DEFINITION,
                    new FunckyDefinition(expression.getFile(), expression.getLine(), reference.getName(), expression),
                    expression.getFile(), expression.getLine(), 1);
        } catch (final CompilationException e) {
            throw new SneakyFunckyRuntimeException(new FunckyRuntimeException(e));
        }
    }
}
