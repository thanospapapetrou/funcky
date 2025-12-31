package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyMonad;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType.IO;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType.UNIT;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER;

public final class IO extends FunckyLibrary {
    private final FunckyTypeVariable a = new FunckyTypeVariable(context);
    private final FunckyTypeVariable b = new FunckyTypeVariable(context);
    public final HigherOrderFunction _return = new HigherOrderFunction(context,
            context -> a, IO(context -> a)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyMonad(context, IO(ctx -> arguments.getFirst().getType()).apply(context),
                    () -> arguments.getFirst());
        }
    };
    public final HigherOrderFunction bind = new HigherOrderFunction(context,
            IO(context -> a),
            FUNCTION(context -> a, IO(context -> b)),
            IO(context -> b)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return (FunckyMonad) ((FunckyFunction) arguments.get(1).eval(context))
                    .apply(((FunckyMonad) arguments.getFirst().eval(context)).getBase(), context);
        }
    };
    public final FunckyMonad readCharacter = new FunckyMonad(context,
            IO(CHARACTER).apply(context), () -> {
        try { // TODO do not close stdin
            return new FunckyLiteral(context.getEngine(), new FunckyCharacter(context,
                    (char) new InputStreamReader(System.in).read()));
        } catch (final IOException e) {
            throw new RuntimeException(e); // TODO
        }
    });
    public final HigherOrderFunction writeCharacter = new HigherOrderFunction(context,
            CHARACTER, IO(UNIT)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyMonad(context, IO(UNIT).apply(context), () -> {
                System.out.println(((FunckyCharacter) arguments.getFirst().eval(context)).getValue());
                return new FunckyLiteral(context.getEngine(), new FunckyRecord(context, UNIT.apply(context),
                        List.of()));
            });
        }
    };

    public IO(final FunckyContext context) {
        super(context);
    }
}
