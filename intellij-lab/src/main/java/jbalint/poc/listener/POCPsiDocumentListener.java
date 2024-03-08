package jbalint.poc.listener;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDocumentListener;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class POCPsiDocumentListener implements PsiDocumentListener {
    @Override
    public void documentCreated(@NotNull Document document, @Nullable PsiFile psiFile, @NotNull Project project) {

    }

    @Override
    public void fileCreated(@NotNull PsiFile file, @NotNull Document document) {
        PsiDocumentListener.super.fileCreated(file, document);
    }
}
