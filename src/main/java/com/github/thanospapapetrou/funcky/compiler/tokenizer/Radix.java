package com.github.thanospapapetrou.funcky.compiler.tokenizer;

public enum Radix {
    BINARY(2),
    OCTAL(8),
    DECIMAL(10),
    HEXADECIMAL(16);

    private final int radix;

    Radix(final int radix) {
        this.radix = radix;
    }

    public int getRadix() {
        return radix;
    }
}
