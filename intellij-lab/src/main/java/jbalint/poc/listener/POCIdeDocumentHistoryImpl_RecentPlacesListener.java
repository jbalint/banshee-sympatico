package jbalint.poc.listener;

import com.intellij.openapi.fileEditor.impl.IdeDocumentHistoryImpl;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectLocator;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

public class POCIdeDocumentHistoryImpl_RecentPlacesListener implements IdeDocumentHistoryImpl.RecentPlacesListener {
    @Override
    public void recentPlaceAdded(IdeDocumentHistoryImpl.@NotNull PlaceInfo changePlace, boolean isChanged) {
        System.out.println("recentPlaceAdded = " + changePlace);
        // this isn't great but I don't see any other way here.
        // https://plugins.jetbrains.com/docs/intellij/project.html#how-to-get-a-project-instance
        Project project = ProjectLocator.getInstance().guessProjectForFile(changePlace.getFile());
        // https://plugins.jetbrains.com/docs/intellij/psi-files.html#how-do-i-get-a-psi-file
        PsiFile psiFile = PsiManager.getInstance(project).findFile(changePlace.getFile());
        // https://plugins.jetbrains.com/docs/intellij/psi-elements.html
        PsiElement elAtCaret = psiFile.findElementAt(changePlace.getCaretPosition().getStartOffset());
        PsiMethod method = PsiTreeUtil.getNonStrictParentOfType(elAtCaret, PsiMethod.class);
//        PsiClass klass = PsiTreeUtil.getNonStrictParentOfType(elAtCaret, PsiClass.class);
        PsiClass klass = (PsiClass) PsiTreeUtil.findFirstParent(elAtCaret,
                x -> x instanceof PsiClass && ((PsiClass) x).getName() != null);
        if (klass != null && method != null) {
            System.out.println("At: " + klass.getQualifiedName() + "." + method.getName());
        } else if (klass != null) {
            System.out.println("At: " + klass.getQualifiedName());
        } else {
            System.out.println("At (both klass and method are null): " + changePlace);
        }
    }

    @Override
    public void recentPlaceRemoved(IdeDocumentHistoryImpl.@NotNull PlaceInfo changePlace, boolean isChanged) {
        System.out.println("recentPlaceRemoved = " + changePlace);
    }
}
