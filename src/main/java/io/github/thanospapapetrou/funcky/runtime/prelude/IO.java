package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter;
import io.github.thanospapapetrou.funcky.runtime.FunckyFunction;
import io.github.thanospapapetrou.funcky.runtime.FunckyMonad;
import io.github.thanospapapetrou.funcky.runtime.FunckyRecord;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType.UNIT;
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER;

public final class IO extends FunckyLibrary {
    private final FunckyTypeVariable a = new FunckyTypeVariable(engine);
    private final FunckyTypeVariable b = new FunckyTypeVariable(engine);
    public final HigherOrderFunction _return = new HigherOrderFunction(engine,
            engine -> a, FunckyMonadicType.IO(engine -> a)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return new FunckyMonad(engine, FunckyMonadicType.IO(engine -> arguments.getFirst().getType()).apply(engine),
                    () -> arguments.getFirst());
        }
    };
    public final HigherOrderFunction bind = new HigherOrderFunction(engine,
            FunckyMonadicType.IO(engine -> a),
            FUNCTION(engine -> a, FunckyMonadicType.IO(engine -> b)),
            FunckyMonadicType.IO(engine -> b)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return (FunckyMonad) ((FunckyFunction) arguments.get(1).eval(context))
                    .apply(((FunckyMonad) arguments.getFirst().eval(context)).getBase(), context);
        }
    };
    public final FunckyMonad readCharacter = new FunckyMonad(engine,
            FunckyMonadicType.IO(CHARACTER).apply(engine), () -> {
        try { // TODO do not close stdin
            return new FunckyLiteral(engine, new FunckyCharacter(engine, (char) new InputStreamReader(System.in).read()));
        } catch (final IOException e) {
            throw new RuntimeException(e); // TODO
        }
    });
    public final HigherOrderFunction writeCharacter = new HigherOrderFunction(engine,
            CHARACTER, FunckyMonadicType.IO(UNIT)) {
        @Override
        public FunckyMonad apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return new FunckyMonad(engine, FunckyMonadicType.IO(UNIT).apply(engine), () -> {
                System.out.println(((FunckyCharacter) arguments.getFirst().eval(context)).getValue());
                return new FunckyLiteral(engine, new FunckyRecord(engine, UNIT.apply(engine),
                        List.of()));
            });
        }
    };

    public IO(final FunckyEngine engine) {
        super(engine);
    }
}
