package io.github.thanospapapetrou.funcky.runtime.prelude;

import javax.script.ScriptContext;
import javax.swing.text.FieldView;

import io.github.thanospapapetrou.funcky.FunckyEngine;
import io.github.thanospapapetrou.funcky.compiler.ast.FunckyLiteral;
import io.github.thanospapapetrou.funcky.compiler.linker.FunckyContext;
import io.github.thanospapapetrou.funcky.runtime.FunckyValue;

public final class Info extends FunckyLibrary {
    public final FunckyValue languageName = get(FunckyEngine.LANGUAGE);
    public final FunckyValue languageVersion = get(FunckyEngine.LANGUAGE_VERSION);
    public final FunckyValue engineName = get(FunckyEngine.ENGINE);
    public final FunckyValue engineVersion = get(FunckyEngine.ENGINE_VERSION);
    public final FunckyValue names = get(FunckyEngine.NAME);
    public final FunckyValue mimeTypes = get(FunckyEngine.PARAMETER_MIME_TYPES);
    public final FunckyValue extensions = get(FunckyEngine.PARAMETER_EXTENSIONS);
    public final FunckyValue threading = get(FunckyEngine.PARAMETER_THREADING);

    public Info(final FunckyContext context) {
        super(context);
    }

    private FunckyValue get(final String key) {
        final FunckyValue value = (FunckyValue) context.getBindings(ScriptContext.GLOBAL_SCOPE).get(key);
        return (value == null) ? context.getEngine().toFuncky("") : value;
    }
}
