package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.FunckyTypeVariable;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;

public final class Lists extends FunckyLibrary {
    private static final String ERROR_HEAD = "Can not get head of empty list";
    private static final String ERROR_TAIL = "Can not get tail of empty list";

    private final FunckyTypeVariable $_a = new FunckyTypeVariable();
    public final HigherOrderFunction $head = new HigherOrderFunction(this, FunckyListType.LIST($_a), $_a) {
        @Override
        protected FunckyValue apply(final List<FunckyExpression> arguments) {
            final FunckyExpression head = ((FunckyList) arguments.getFirst().eval()).getHead();
            if (head == null) {
                throw new SneakyRuntimeException(ERROR_HEAD);
            }
            return head.eval();
        }
    };
    public final HigherOrderFunction $tail = new HigherOrderFunction(this,
            FunckyListType.LIST($_a), FunckyListType.LIST($_a)) {
        @Override
        protected FunckyList apply(final List<FunckyExpression> arguments) {
            final FunckyExpression tail = ((FunckyList) arguments.getFirst().eval()).getTail();
            if (tail == null) {
                throw new SneakyRuntimeException(ERROR_TAIL);
            }
            return (FunckyList) tail.eval();
        }
    };
    public final HigherOrderFunction $prepend = new HigherOrderFunction(this,
            FunckyListType.LIST($_a), $_a, FunckyListType.LIST($_a)) {
        @Override
        protected FunckyList apply(final List<FunckyExpression> arguments) {
            return new FunckyList(
                    (FunckyListType) arguments.get(0).getType().unify(FunckyListType.LIST(arguments.get(1).getType())),
                    arguments.get(1), arguments.get(0));
        }
    };

    public Lists(final FunckyEngine engine) {
        super(engine);
    }
}
