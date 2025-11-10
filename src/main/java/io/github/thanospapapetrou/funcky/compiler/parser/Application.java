package io.github.thanospapapetrou.funcky.compiler.parser;

public final class Application extends Expression {
    private static final String FORMAT_APPLICATION = "%1$s %2$s";
    private static final String FORMAT_NESTED_APPLICATION = "%1$s (%2$s)";

    private final Expression function;
    private final Expression argument;

    public Application(final Expression function, final Expression argument) {
        super(function.file, function.line, function.column);
        this.function = function;
        this.argument = argument;
    }

    public Expression getFunction() {
        return function;
    }

    public Expression getArgument() {
        return argument;
    }

    @Override
    public String toString() {
        return String.format((argument instanceof Application) ? FORMAT_NESTED_APPLICATION : FORMAT_APPLICATION,
                function, argument);
    }
}
