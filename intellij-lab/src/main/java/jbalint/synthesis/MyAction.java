package jbalint.synthesis;

import com.complexible.stardog.api.Connection;
import com.complexible.stardog.api.ConnectionConfiguration;
import com.complexible.stardog.api.DriverManager;
import com.intellij.ide.highlighter.ArchiveFileType;
import com.intellij.ide.highlighter.JavaClassFileType;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompilerManager;
import com.intellij.openapi.editor.CaretModel;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.OrderEntry;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.jbalint.jcfl.ClassFile;
import com.jbalint.jcfl.ClassFileParser;
import com.jbalint.jora.proto.ClassFileToRdf;
import com.stardog.stark.Statement;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.Set;

/**
 * TODO : doc me
 * 1234567
 */
public class MyAction extends AnAction {

    private static final Date LOAD_TIME = new Date();

    static DriverManager x;

    /**
     * Initialize {@link DriverManager} with the classloader that will allow it to find Guice modules
     * (UGH, Stardog...)
     */
    static {
        ClassLoader threadContextClassLoader = Thread.currentThread().getContextClassLoader();
        try {
            Thread.currentThread().setContextClassLoader(MyAction.class.getClassLoader());
            DriverManager.getInstance();
        }
        finally {
            Thread.currentThread().setContextClassLoader(threadContextClassLoader);
        }
    }

    // TODO : should use `https://localhost/stardog/x`. needs fix for #9003
    Connection conn;// = ConnectionConfiguration.from("http://tagore/jora-sypet").connect();

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        if (conn == null) {
            conn = ConnectionConfiguration.from("http://tagore/jora-sypet").connect();
        }
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

//        final Editor editor = e.getRequiredData(CommonDataKeys.EDITOR);
        final Editor editor = e.getData(CommonDataKeys.EDITOR);
        if (editor == null) {
            return;
        }
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

//        PsiManager.getInstance(project).findFile(virtualFile);

        CompilerManager instance = CompilerManager.getInstance(proj);
        instance.addBeforeTask(x -> {
            return true;
        });

        proj.getPresentableUrl();
        proj.getBasePath();
//        instance.addBeforeTask();

        {
            Project project = proj;
            Module[] modules = ModuleManager.getInstance(project).getModules();

            conn.begin();
            for (Module module : modules) {
                System.err.println("MODULE: " + module);
                final ModuleRootManager moduleRootManager = ModuleRootManager.getInstance(module);
                final OrderEntry[] orderEntries = moduleRootManager.getOrderEntries();
                for (final OrderEntry orderEntry : orderEntries) {
                    handleFiles(orderEntry.getFiles(OrderRootType.CLASSES));
//                    System.err.println(orderEntry.getFiles(OrderRootType.CLASSES));
//                    System.err.println(Arrays.stream(orderEntry.getFiles(OrderRootType.CLASSES))
//                            .map(Object::toString).collect(Collectors.joining(",\n")));
                }
            }
            conn.commit();
        }
    }

    void handleFile(VirtualFile vf) {
        if (vf.getFileType() == ArchiveFileType.INSTANCE && "jar".equalsIgnoreCase(vf.getExtension())) {

        }
        if (vf.getFileType() instanceof JavaClassFileType) {
            try {
                ClassFile cf = ClassFileParser.parse(vf.getInputStream());
                Set<Statement> statements = ClassFileToRdf.toRdf(cf);
                conn.add().graph(statements);
                conn.commit();
                conn.begin();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        else {
            handleFiles(vf.getChildren());
        }
    }

    void handleFiles(VirtualFile vfs[]) {
        Arrays.stream(vfs).forEach(this::handleFile);
    }
}
