package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;

public final class Lists extends FunckyLibrary {
    private static final String ERROR_HEAD = "Can not get head of empty list";
    private static final String ERROR_TAIL = "Can not get tail of empty list";

    private final FunckyTypeVariable a = new FunckyTypeVariable(engine);
    public final HigherOrderFunction head = new HigherOrderFunction(engine, LIST(engine -> a), engine -> a) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            final FunckyExpression head = ((FunckyList) arguments.getFirst().eval(context)).getHead();
            if (head == null) {
                throw new SneakyRuntimeException(ERROR_HEAD);
            }
            return head.eval(context);
        }
    };
    public final HigherOrderFunction tail = new HigherOrderFunction(engine,
            LIST(engine -> a), LIST(engine -> a)) {
        @Override
        public FunckyList apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            final FunckyExpression tail = ((FunckyList) arguments.getFirst().eval(context)).getTail();
            if (tail == null) {
                throw new SneakyRuntimeException(ERROR_TAIL);
            }
            return (FunckyList) tail.eval(context);
        }
    };
    public final HigherOrderFunction prepend = new HigherOrderFunction(engine,
            LIST(engine -> a), engine -> a, LIST(engine -> a)) {
        @Override
        public FunckyList apply(final List<FunckyExpression> arguments, final ScriptContext context) {
            return new FunckyList(engine, (FunckyListType) arguments.get(0).getType()
                    .unify(LIST(engine -> arguments.get(1).getType()).apply(engine)), arguments.get(1),
                    arguments.get(0));
        }
    };

    public Lists(final FunckyEngine engine) {
        super(engine);
    }
}
