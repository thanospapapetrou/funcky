package io.github.thanospapapetrou.funcky.runtime.prelude;

import io.github.thanospapapetrou.funcky.FunckyEngine;

public final class Lists extends FunckyLibrary {
    //    private static final FunckyTypeVariable A = new FunckyTypeVariable();
    //    public static final HigherOrderFunction HEAD = new HigherOrderFunction(Lists.class, "head",
    //            new FunckyListType(A), A) {
    //        @Override
    //        protected FunckyValue apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            final FunckyExpression head = ((FunckyList) arguments.getFirst().eval(context)).getHead();
    //            if (head == null) {
    //                throw new SneakyRuntimeException(ERROR_HEAD);
    //            }
    //            return head.eval(context);
    //        }
    //    };
    //    public static final HigherOrderFunction TAIL = new HigherOrderFunction(Lists.class, "tail",
    //            new FunckyListType(A), new FunckyListType(A)) {
    //        @Override
    //        protected FunckyList apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //            final FunckyExpression tail = ((FunckyList) arguments.getFirst().eval(context)).getTail();
    //            if (tail == null) {
    //                throw new SneakyRuntimeException(ERROR_TAIL);
    //            }
    //            return (FunckyList) tail.eval(context);
    //        }
    //    };
    //    public static final HigherOrderFunction PREPEND = new HigherOrderFunction(Lists.class, "prepend",
    //            new FunckyListType(A), A, new FunckyListType(A)) {
    //        @Override
    //        protected FunckyList apply(final ScriptContext context, final List<FunckyExpression> arguments) {
    //                return new FunckyList((FunckyListType) arguments.get(0).getType()
    //                        .unify(new FunckyListType(arguments.get(1).getType())),
    //                        arguments.get(1), arguments.get(0));
    //        }
    //    };
    //    private static final String ERROR_HEAD = "Can not get head of empty list";
    //    private static final String ERROR_TAIL = "Can not get tail of empty list";
    //
    //    public Lists() {
    //        super(HEAD, TAIL, PREPEND);
    //    }
    public Lists(final FunckyEngine engine) {
        super(engine);
    }
}
