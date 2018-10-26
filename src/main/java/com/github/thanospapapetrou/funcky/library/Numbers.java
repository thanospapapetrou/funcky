package com.github.thanospapapetrou.funcky.library;

import com.github.thanospapapetrou.funcky.script.expression.Expression;
import com.github.thanospapapetrou.funcky.script.expression.literal.Function;
import com.github.thanospapapetrou.funcky.script.expression.literal.Literal;
import com.github.thanospapapetrou.funcky.script.expression.literal.Number;
import com.github.thanospapapetrou.funcky.script.expression.literal.type.FunctionType;
import com.github.thanospapapetrou.funcky.script.expression.literal.type.SimpleType;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.script.ScriptContext;
import javax.script.ScriptException;

public class Numbers extends Library {
    public static final SimpleType NUMBER = new SimpleType("Number");
    // TODO minus = subtract 0
    private static final Set<Literal> LITERALS =
            Collections.unmodifiableSet(new HashSet<Literal>() {
                private static final long serialVersionUID = 0L;

                {
                    add(NUMBER);
                    add(new Number(Double.NaN));
                    add(new Number(Double.POSITIVE_INFINITY));
                    add(new Function("minus", new FunctionType(NUMBER, NUMBER)) {
                        @Override
                        public Literal apply(final ScriptContext context,
                                final Expression argument) {
                            try {
                                return new Number(-((Number) argument.eval()).getValue());
                            } catch (final ScriptException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                                return null;
                            }
                        }
                    });
                    add(new BinaryArithmeticOperator("add") {
                        @Override
                        protected double apply(final double a, final double b) {
                            return a + b;
                        }
                    });
                    add(new BinaryArithmeticOperator("subtract") {
                        @Override
                        protected double apply(final double a, final double b) {
                            return a - b;
                        }
                    });
                    add(new BinaryArithmeticOperator("multiply") {
                        @Override
                        protected double apply(final double a, final double b) {
                            return a * b;
                        }
                    });
                    add(new BinaryArithmeticOperator("divide") {
                        @Override
                        protected double apply(final double a, final double b) {
                            return a / b;
                        }
                    });
                    add(new BinaryArithmeticOperator("modulo") {
                        @Override
                        protected double apply(final double a, final double b) {
                            return a % b;
                        }
                    });

                }
            });

    public Numbers() {
        super(LITERALS);
    }
}