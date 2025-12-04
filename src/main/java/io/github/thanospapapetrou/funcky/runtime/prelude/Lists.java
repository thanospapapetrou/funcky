package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import javax.script.ScriptContext;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;

public final class Lists extends FunckyLibrary {
    private static final String ERROR_HEAD = "Can not get head of empty list";
    private static final String ERROR_TAIL = "Can not get tail of empty list";

    private final FunckyTypeVariable $_a = new FunckyTypeVariable(engine);
    public final HigherOrderFunction $head = new HigherOrderFunction(engine, this, LIST(engine -> $_a), engine -> $_a) {
        @Override
        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyExpression head = ((FunckyList) arguments.getFirst().eval(context)).getHead();
            if (head == null) {
                throw new SneakyRuntimeException(ERROR_HEAD);
            }
            return head.eval(context);
        }
    };
    public final HigherOrderFunction $tail = new HigherOrderFunction(engine, this,
            LIST(engine -> $_a), LIST(engine -> $_a)) {
        @Override
        protected FunckyList apply(final ScriptContext context, final List<FunckyExpression> arguments) {
            final FunckyExpression tail = ((FunckyList) arguments.getFirst().eval(context)).getTail();
            if (tail == null) {
                throw new SneakyRuntimeException(ERROR_TAIL);
            }
            return (FunckyList) tail.eval(context);
        }
    };
    public final HigherOrderFunction $prepend = new HigherOrderFunction(engine, this,
            LIST(engine -> $_a), engine -> $_a, LIST(engine -> $_a)) {
        @Override
        protected FunckyList apply(final ScriptContext context, final List<FunckyExpression> arguments) {
                return new FunckyList(engine, (FunckyListType) arguments.get(0).getType()
                        .unify(new FunckyListType(engine, new FunckyLiteral(engine, arguments.get(1).getType()))),
                        arguments.get(1), arguments.get(0));
        }
    };

    public Lists(final FunckyEngine engine) {
        super(engine);
    }
}
