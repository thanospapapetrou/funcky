package io.github.thanospapapetrou.funcky.compiler.parser;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

public record Script(URI file, List<Import> imports, List<Definition> definitions) {
    public Script(final URI file) {
        this(file, new ArrayList<>(), new ArrayList<>());
    }

    @Override
    public String toString() {
        return file().toString();
    }
}
