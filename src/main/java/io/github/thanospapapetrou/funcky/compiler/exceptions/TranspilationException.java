package io.github.thanospapapetrou.funcky.compiler.exceptions;

import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;

public final class TranspilationException extends FunckyCompilationException {
    private static final String FORMAT =
            "%1$s %2$s %3$s (file: %4$s %5$s, line: %6$d, column: %7$d, position: %8$d, start: %9$d, end: %10$d)%n";
    private static final String MESSAGE = "Transpilation errors%n%1$s";

    private static String format(final Diagnostic<? extends JavaFileObject> diagnostic) {
        return String.format(String.format(FORMAT, diagnostic.getKind(), diagnostic.getCode(),
                diagnostic.getMessage(Locale.ROOT),
                // TODO ommit nulls
                (diagnostic.getSource() == null) ? null : diagnostic.getSource().getKind(),
                (diagnostic.getSource() == null) ? null : diagnostic.getSource().getName(), diagnostic.getLineNumber(),
                diagnostic.getColumnNumber(),
                diagnostic.getPosition(), diagnostic.getStartPosition(), diagnostic.getEndPosition()));
    }

    public TranspilationException(final List<Diagnostic<? extends JavaFileObject>> diagnostics) {
        super(String.format(MESSAGE, diagnostics.stream()
                .map(TranspilationException::format)
                .collect(Collectors.joining())), null, -1, -1);
    }
}
