package com.github.thanospapapetrou.funcky.parser.tokenizer;

import java.io.IOException;
import java.net.URI;

class LineIteratorException extends RuntimeException {
    private static final String ERROR_READING = "Error reading from %1$s";
    private static final long serialVersionUID = 0L;

    LineIteratorException(final URI file, final IOException e) {
        super(String.format(ERROR_READING, file), e);
    }
}