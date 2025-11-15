package io.github.thanospapapetrou.funcky.compiler.ast;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import io.github.thanospapapetrou.funcky.compiler.linker.FunckyScript;
import io.github.thanospapapetrou.funcky.compiler.linker.Linker;

public record Script(URI file, List<Import> imports, List<Definition> definitions) {
    public Script(final URI file) {
        this(file, new ArrayList<>(), new ArrayList<>());
    }

    public Script(final Expression expression) {
        this(Linker.STDIN);
        definitions.add(new Definition(Linker.STDIN, 1, FunckyScript.IT, expression));
    }

    @Override
    public String toString() {
        return file().toString();
    }
}
