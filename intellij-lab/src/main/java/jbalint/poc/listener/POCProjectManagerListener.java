package jbalint.poc.listener;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.project.ProjectManagerListener;
import com.intellij.psi.search.FilenameIndex;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Date;

public class POCProjectManagerListener implements ProjectManagerListener {
    @Override
    public void projectOpened(@NotNull Project project) {
        System.out.println("Opened: " + project.getName());
        ProjectManagerListener.super.projectOpened(project);
        File f = new File("/tmp/idea-plugin-fiddling.log");
//        Project defaultProject = ProjectManager.getInstance().getOpenProjects()[0];
        @NotNull String[] allFilenames = FilenameIndex.getAllFilenames(project);
//        System.out.println(Arrays.toString(allFilenames));
        try {
            FileWriter w = new FileWriter(f);
            w.write("Hi from " + new Date() + "\n");
            w.flush();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void projectClosed(@NotNull Project project) {
        ProjectManagerListener.super.projectClosed(project);
    }

    @Override
    public void projectClosing(@NotNull Project project) {
        ProjectManagerListener.super.projectClosing(project);
    }

    @Override
    public void projectClosingBeforeSave(@NotNull Project project) {
        ProjectManagerListener.super.projectClosingBeforeSave(project);
    }
}
