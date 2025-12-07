open module io.github.thanospapapetrou.funcky {
    requires java.logging;
    requires java.scripting;
    requires java.desktop;
    exports io.github.thanospapapetrou.funcky;
    exports io.github.thanospapapetrou.funcky.compiler;
    exports io.github.thanospapapetrou.funcky.compiler.ast;
    exports io.github.thanospapapetrou.funcky.compiler.linker;
    exports io.github.thanospapapetrou.funcky.runtime;
    exports io.github.thanospapapetrou.funcky.runtime.exceptions;
    exports io.github.thanospapapetrou.funcky.runtime.types;
}
