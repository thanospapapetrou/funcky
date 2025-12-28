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
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyMonadicType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

public final class IO extends FunckyLibrary {
    private final FunckyTypeVariable a = new FunckyTypeVariable(engine);
    private final FunckyTypeVariable b = new FunckyTypeVariable(engine);
    public final HigherOrderFunction _return = new HigherOrderFunction(engine,
            engine -> a, engine -> FunckyMonadicType.io(engine, new FunckyLiteral(engine, a))) {
        @Override
        public FunckyMonad apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyMonad(engine,
                    FunckyMonadicType.io(engine, new FunckyLiteral(engine, arguments.getFirst().getType())),
                    () -> arguments.getFirst());
        }
    };
    public final HigherOrderFunction bind = new HigherOrderFunction(engine,
            engine -> FunckyMonadicType.io(engine, new FunckyLiteral(engine, a)),
            FunckyFunctionType.FUNCTION(engine -> a, engine -> FunckyMonadicType.io(engine, new FunckyLiteral(engine,
                    b))), engine -> FunckyMonadicType.io(engine, new FunckyLiteral(engine, b))) {
        @Override
        public FunckyMonad apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return (FunckyMonad) ((FunckyFunction) arguments.get(1).eval(context))
                    .apply(((FunckyMonad) arguments.getFirst().eval(context)).getBase(), context);
        }
    };
    public final FunckyMonad readCharacter = new FunckyMonad(engine, FunckyMonadicType.io(engine,
            new FunckyLiteral(engine, FunckySimpleType.CHARACTER.apply(engine))), () -> {
        try { // TODO do not close stdin
            return new FunckyLiteral(engine, new FunckyCharacter(engine, (char) new InputStreamReader(System.in).read()));
        } catch (final IOException e) {
            throw new RuntimeException(e); // TODO
        }
    });
    public final HigherOrderFunction writeCharacter = new HigherOrderFunction(engine,
            FunckySimpleType.CHARACTER, engine -> FunckyMonadicType.io(engine,
            new FunckyLiteral(engine, FunckyRecordType.UNIT.apply(engine)))) {
        @Override
        public FunckyMonad apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            return new FunckyMonad(engine, FunckyMonadicType.io(engine,
                    new FunckyLiteral(engine, FunckyRecordType.UNIT.apply(engine))),
                    () -> {
                        System.out.println(((FunckyCharacter) arguments.getFirst().eval(context)).getValue());
                        return new FunckyLiteral(engine, new FunckyRecord(engine,
                                FunckyRecordType.UNIT.apply(engine), List.of()));
                    });
        }
    };

    public IO(final FunckyEngine engine) {
        super(engine);
    }
}
