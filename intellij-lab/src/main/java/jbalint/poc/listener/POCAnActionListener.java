package jbalint.poc.listener;

import com.intellij.codeInsight.navigation.actions.GotoDeclarationAction;
import com.intellij.ide.impl.dataRules.PsiFileRule;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.actionSystem.ex.AnActionListener;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class POCAnActionListener implements AnActionListener {
    @Override
    public void beforeActionPerformed(@NotNull AnAction action, @NotNull AnActionEvent event) {
        AnActionListener.super.beforeActionPerformed(action, event);

        if (action instanceof GotoDeclarationAction) {
            PsiFile psiFile = event.getDataContext().getData(CommonDataKeys.PSI_FILE);
            System.out.println("Action: " + action.toString() + " --- on: " + psiFile);
        }
    }

    @Override
    public void afterActionPerformed(@NotNull AnAction action, @NotNull AnActionEvent event, @NotNull AnActionResult result) {
        AnActionListener.super.afterActionPerformed(action, event, result);
    }

    @Override
    public void beforeEditorTyping(char c, @NotNull DataContext dataContext) {
        AnActionListener.super.beforeEditorTyping(c, dataContext);
    }

    @Override
    public void afterEditorTyping(char c, @NotNull DataContext dataContext) {
        AnActionListener.super.afterEditorTyping(c, dataContext);
    }

    @Override
    public void beforeShortcutTriggered(@NotNull Shortcut shortcut, @NotNull List<AnAction> actions, @NotNull DataContext dataContext) {
        AnActionListener.super.beforeShortcutTriggered(shortcut, actions, dataContext);
    }

    @Override
    public void beforeActionPerformed(@NotNull AnAction action, @NotNull DataContext dataContext, @NotNull AnActionEvent event) {
        AnActionListener.super.beforeActionPerformed(action, dataContext, event);
    }

    @Override
    public void afterActionPerformed(@NotNull AnAction action, @NotNull DataContext dataContext, @NotNull AnActionEvent event) {
        AnActionListener.super.afterActionPerformed(action, dataContext, event);
    }
}
