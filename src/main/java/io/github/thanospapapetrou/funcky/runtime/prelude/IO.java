package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.List;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyMonad;
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType.IO;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType.UNIT;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER;

public final class IO extends FunckyLibrary {
    private static final int BUFFER_SIZE = 1024;
    private static final int READER_STDIN = 0;
    private static final int WRITER_STDERR = 2;
    private static final int WRITER_STDOUT = 1;

    private final FunckyTypeVariable a = new FunckyTypeVariable(context);
    private final FunckyTypeVariable b = new FunckyTypeVariable(context);
    private final FunckyLiteral nil = new FunckyLiteral(new FunckyRecord(context));
    public final HigherOrderFunction _return = new HigherOrderFunction(context, a, IO(a)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyMonad(context, IO(arguments.getFirst().getType()).apply(context), arguments::getFirst);
        }
    };
    public final HigherOrderFunction bind = new HigherOrderFunction(context, IO(a), FUNCTION(a, IO(b)), IO(b)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return (FunckyMonad) ((FunckyFunction) arguments.get(1).eval(context))
                    .apply(((FunckyMonad) arguments.getFirst().eval(context)).getBase(), context);
        }
    };
    public final HigherOrderFunction readCharacter = new HigherOrderFunction(context, NUMBER, IO(CHARACTER)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyMonad(context, IO(CHARACTER).apply(context), () -> {
                try {
                    return new FunckyLiteral(new FunckyCharacter(context,
                            (char) getReader(((FunckyNumber) arguments.getFirst().eval(context)).getValue().intValue())
                                    .read()));
                } catch (final IOException e) {
                    throw new RuntimeException("Error reading character", e);
                }
            });
        }
    };
    public final HigherOrderFunction writeCharacter = new HigherOrderFunction(context, NUMBER, CHARACTER, IO(UNIT)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyMonad(context, IO(UNIT).apply(context), () -> {
                try {
                    getWriter(((FunckyNumber) arguments.getFirst().eval(context)).getValue().intValue())
                            .write(((FunckyCharacter) arguments.get(1).eval(context)).getValue());
                    return nil;
                } catch (final IOException e) {
                    throw new RuntimeException("Error writing character", e);
                }
            });
        }
    };
    public final HigherOrderFunction readString = new HigherOrderFunction(context, NUMBER, IO(STRING)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyMonad(context, IO(STRING).apply(context), () -> {
                final StringBuilder string = new StringBuilder();
                final char[] buffer = new char[BUFFER_SIZE];
                int read;
                try {
                    while ((read = getReader(((FunckyNumber) arguments.getFirst().eval(context)).getValue().intValue())
                            .read(buffer, 0, BUFFER_SIZE)) != -1) {
                        string.append(buffer, 0, read);
                    }
                    return new FunckyLiteral(context.getEngine().toFuncky(string.toString()));
                } catch (final IOException e) {
                    throw new RuntimeException("Error reading string", e);
                }
            });
        }
    };
    public final HigherOrderFunction writeString = new HigherOrderFunction(context, NUMBER, STRING, IO(UNIT)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyMonad(context, IO(UNIT).apply(context), () -> {
                try {
                    getWriter(((FunckyNumber) arguments.getFirst().eval(context)).getValue().intValue())
                            .write(arguments.get(1).eval(context).toString());
                    return nil;
                } catch (final IOException e) {
                    throw new RuntimeException("Error writing string", e);
                }
            });
        }
    };
    public final HigherOrderFunction flush = new HigherOrderFunction(context, NUMBER, IO(UNIT)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyMonad(context, IO(UNIT).apply(context), () -> {
                try {
                    getWriter(((FunckyNumber) arguments.getFirst().eval(context)).getValue().intValue()).flush();
                    return nil;
                } catch (final IOException e) {
                    throw new RuntimeException("Error flushing", e);
                }
            });
        }
    };

    public IO(final FunckyContext context) {
        super(context);
    }

    private Reader getReader(final int reader) {
        if (reader == READER_STDIN) {
            return context.getReader();
        }
        throw new RuntimeException("Invalid reader " + reader);
    }

    private Writer getWriter(final int writer) {
        return switch (writer) {
            case WRITER_STDOUT -> context.getWriter();
            case WRITER_STDERR -> context.getErrorWriter();
            default -> throw new RuntimeException("Invalid writer " + writer);
        };
    }
}
