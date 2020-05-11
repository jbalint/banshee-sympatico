package jbalint.synthesis;

import com.intellij.openapi.compiler.CompileContext;
import com.intellij.openapi.compiler.CompileTask;

public class MyCompileTask implements CompileTask {
    @Override
    public boolean execute(CompileContext context) {
        return true;
    }
}
