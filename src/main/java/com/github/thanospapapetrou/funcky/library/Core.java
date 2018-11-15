package com.github.thanospapapetrou.funcky.library;

import com.github.thanospapapetrou.funcky.script.expression.Expression;
import com.github.thanospapapetrou.funcky.script.expression.Reference;
import com.github.thanospapapetrou.funcky.script.expression.literal.Function;
import com.github.thanospapapetrou.funcky.script.expression.literal.Literal;
import com.github.thanospapapetrou.funcky.script.expression.literal.Number;
import com.github.thanospapapetrou.funcky.script.expression.literal.type.FunctionType;
import com.github.thanospapapetrou.funcky.script.expression.literal.type.SimpleType;
import com.github.thanospapapetrou.funcky.script.expression.literal.type.Type;
import com.github.thanospapapetrou.funcky.script.expression.literal.type.TypeVariable;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.script.ScriptContext;

public class Core extends Library {
    /**
     * The type of types.
     */
    public static final SimpleType TYPE = new SimpleType("Type");

    /**
     * Function type constructor.
     */
    public static final Functor FUNCTION = new Functor(new Reference("Function"),
            new FunctionType(TYPE, new FunctionType(TYPE, TYPE)), 2) {
        @Override
        protected FunctionType apply(final ScriptContext context, final Expression... arguments) {
            return new FunctionType((Type) arguments[0].eval(context),
                    (Type) arguments[1].eval(context));
        }
    };

    private static final Set<Literal> LITERALS =
            Collections.unmodifiableSet(new HashSet<Literal>() {
                private static final long serialVersionUID = 0L;

                {
                    // equals
                    final TypeVariable a = new TypeVariable(); // TODO add name to type variable
                    add(new Functor(new Reference("equals"),
                            new FunctionType(a, new FunctionType(a, Numbers.NUMBER)), 2) {
                        @Override
                        protected Literal apply(final ScriptContext context,
                                final Expression... arguments) {
                            // TODO return boolean
                            return new Number(arguments[0].equals(arguments[1]) ? 1.0 : 0.0);
                        }
                    });
                    // Function
                    add(FUNCTION);
                    // Type
                    add(TYPE);
                    // typeOf
                    // TODO add name to type variable
                    add(new Function(new Reference("typeOf"),
                            new FunctionType(new TypeVariable(), TYPE)) {
                        @Override
                        public Type apply(final ScriptContext context, final Expression argument) {
                            return argument.getType(context);
                        }
                    });
                }
            });
    // TODO equals
    // TODO if

    public Core() {
        super(LITERALS);
    }
}
