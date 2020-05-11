package jbalint.sythesis;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompilerManager;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.editor.CaretModel;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.roots.FileIndexUtil;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.OrderEntry;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.impl.file.impl.FileManager;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Date;
import java.util.stream.Collectors;

/**
 * TODO : doc me
 * 1234567
 */
public class MyAction extends AnAction {

    private static final Date LOAD_TIME = new Date();

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        Project proj = ProjectManager.getInstance().getOpenProjects()[0];
        PsiManager psiManager = PsiManager.getInstance(proj);
        ApplicationManager.getApplication();
//        FilenameIndex.processAllFileNames();
//        ServiceManager.getService()

        FileEditorManager manager = FileEditorManager.getInstance(proj);

        VirtualFile files[] = manager.getSelectedFiles();
        System.err.println(files);
        System.err.println("The action has been performed on version: " + LOAD_TIME);

        // https://intellij-support.jetbrains.com/hc/en-us/community/posts/206795775-Get-current-Project-current-file-in-editor

        final Editor editor = e.getRequiredData(CommonDataKeys.EDITOR);
        final CaretModel caretModel = editor.getCaretModel();

        PsiFile pf = DataKeys.PSI_FILE.getData(e.getDataContext());
        Editor editor2 = DataKeys.EDITOR.getData(e.getDataContext());
        PsiElement pe = pf.findElementAt(editor2.getCaretModel().getOffset());
//        PsiMethod method = findMethod(pe);

        PsiElement element = pf.findElementAt(editor2.getCaretModel().getOffset());
        PsiMethod containingMethod = PsiTreeUtil.getParentOfType(element, PsiMethod.class);
        PsiClass containingClass = null;
        if (containingMethod != null) {
            containingClass = containingMethod.getContainingClass();
        }

        System.err.println(containingClass);

        CompilerManager instance = CompilerManager.getInstance(proj);

        proj.getPresentableUrl();
        proj.getBasePath();
//        instance.addBeforeTask();

        {
            Project project = proj;
            Module[] modules = ModuleManager.getInstance(project).getModules();

            for (Module module : modules) {
                System.err.println("MODULE: " + module);
                final ModuleRootManager moduleRootManager = ModuleRootManager.getInstance(module);
                final OrderEntry[] orderEntries = moduleRootManager.getOrderEntries();
                for (final OrderEntry orderEntry : orderEntries) {
                    System.err.println(orderEntry.getFiles(OrderRootType.CLASSES));
                    System.err.println(Arrays.stream(orderEntry.getFiles(OrderRootType.CLASSES))
                            .map(Object::toString).collect(Collectors.joining(",\n")));
                }
            }
        }
    }
}
