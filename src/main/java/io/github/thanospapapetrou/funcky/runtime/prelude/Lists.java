package io.github.thanospapapetrou.funcky.runtime.prelude;

import java.util.List;

import io.github.thanospapapetrou.funcky.compiler.ast.FunckyExpression;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyList;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;
import io.github.thanospapapetrou.funcky.runtime.exceptions.SneakyRuntimeException;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType;
import io.github.thanospapapetrou.funcky.runtime.types.FunckyTypeVariable;

import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST;

public final class Lists extends FunckyLibrary {
    private static final String ERROR_HEAD = "Can not get head of empty list";
    private static final String ERROR_TAIL = "Can not get tail of empty list";

    private final FunckyTypeVariable a = new FunckyTypeVariable(context);
    public final HigherOrderFunction head = new HigherOrderFunction(context, LIST(context -> a), context -> a) {
        @Override
        public FunckyValue apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            final FunckyExpression head = ((FunckyList) arguments.getFirst().eval(context)).getHead();
            if (head == null) {
                throw new SneakyRuntimeException(ERROR_HEAD);
            }
            return head.eval(context);
        }
    };
    public final HigherOrderFunction tail = new HigherOrderFunction(context,
            LIST(context -> a), LIST(context -> a)) {
        @Override
        public FunckyList apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            final FunckyExpression tail = ((FunckyList) arguments.getFirst().eval(context)).getTail();
            if (tail == null) {
                throw new SneakyRuntimeException(ERROR_TAIL);
            }
            return (FunckyList) tail.eval(context);
        }
    };
    public final HigherOrderFunction prepend = new HigherOrderFunction(context,
            LIST(context -> a), context -> a, LIST(context -> a)) {
        @Override
        public FunckyList apply(final List<FunckyExpression> arguments, final FunckyContext context) {
            return new FunckyList(context, (FunckyListType) arguments.get(0).getType()
                    .unify(LIST(ctx -> arguments.get(1).getType()).apply(context)), arguments.get(1),
                    arguments.get(0));
        }
    };

    public Lists(final FunckyContext context) {
        super(context);
    }
}
