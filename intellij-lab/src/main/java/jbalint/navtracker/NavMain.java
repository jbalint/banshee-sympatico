package jbalint.navtracker;

import com.intellij.codeInsight.completion.CompletionPhaseListener;
import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.codeInsight.daemon.impl.DaemonListeners;
import com.intellij.codeInsight.daemon.impl.EditorTrackerListener;
import com.intellij.codeInsight.lookup.Lookup;
import com.intellij.codeInsight.lookup.LookupManagerListener;
import com.intellij.codeInspection.ex.InspectListener;
import com.intellij.codeInspection.ex.InspectionToolWrapper;
import com.intellij.find.FindModelListener;
import com.intellij.ide.actions.BackAction;
import com.intellij.ide.navigationToolbar.NavBarModelListener;
import com.intellij.ide.projectView.impl.AbstractProjectViewPane;
import com.intellij.ide.projectView.impl.ProjectViewListener;
import com.intellij.ide.ui.search.ComponentHighlightingListener;
import com.intellij.lang.annotation.AnnotationSession;
import com.intellij.navigation.DirectNavigationProvider;
import com.intellij.navigation.SymbolNavigationProvider;
import com.intellij.openapi.actionSystem.ActionGroup;
import com.intellij.openapi.actionSystem.ActionManagerListener;
import com.intellij.openapi.actionSystem.ActionToolbar;
import com.intellij.openapi.command.CommandEvent;
import com.intellij.openapi.command.CommandListener;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorNavigationDelegate;
import com.intellij.openapi.fileEditor.FileDocumentManagerListener;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.impl.IdeDocumentHistoryImpl;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Collection;
import java.util.List;

public class NavMain {
    public static void main(String[] args) {
        EditorNavigationDelegate d;
        DirectNavigationProvider d2;
        SymbolNavigationProvider snp;

        CompletionPhaseListener cpl;

        new EditorTrackerListener() {
            @Override
            public void activeEditorsChanged(@NotNull List<? extends Editor> activeEditors) {

            }
        };

        new LookupManagerListener() {
            @Override
            public void activeLookupChanged(@Nullable Lookup oldLookup, @Nullable Lookup newLookup) {

            }
        };

        new InspectListener() {
            @Override
            public void inspectionFinished(long duration, long threadId, int problemsCount, InspectionToolWrapper<?, ?> tool, InspectionKind kind, @Nullable PsiFile file, Project project) {
                InspectListener.super.inspectionFinished(duration, threadId, problemsCount, tool, kind, file, project);
            }

            @Override
            public void activityFinished(long duration, long threadId, ActivityKind activityKind, Project project) {
                InspectListener.super.activityFinished(duration, threadId, activityKind, project);
            }

            @Override
            public void fileAnalyzed(PsiFile file, Project project) {
                InspectListener.super.fileAnalyzed(file, project);
            }
        };

        new FindModelListener() {
            @Override
            public void findNextModelChanged() {

            }
        };

        new NavBarModelListener() {
            @Override
            public void modelChanged() {

            }

            @Override
            public void selectionChanged() {

            }
        };

        new ProjectViewListener() {
            @Override
            public void paneShown(@NotNull AbstractProjectViewPane current, @Nullable AbstractProjectViewPane previous) {
                ProjectViewListener.super.paneShown(current, previous);
            }
        };

        new ComponentHighlightingListener() {
            @Override
            public void highlight(@NotNull JComponent component, @NotNull String searchString) {

            }
        };

        new ActionManagerListener() {
            @Override
            public void toolbarCreated(@NotNull String place, @NotNull ActionGroup group, boolean horizontal, @NotNull ActionToolbar toolbar) {
                ActionManagerListener.super.toolbarCreated(place, group, horizontal, toolbar);
            }
        };

        new CommandListener() {
            @Override
            public void commandStarted(@NotNull CommandEvent event) {
                CommandListener.super.commandStarted(event);
            }

            @Override
            public void beforeCommandFinished(@NotNull CommandEvent event) {
                CommandListener.super.beforeCommandFinished(event);
            }

            @Override
            public void commandFinished(@NotNull CommandEvent event) {
                CommandListener.super.commandFinished(event);
            }

            @Override
            public void undoTransparentActionStarted() {
                CommandListener.super.undoTransparentActionStarted();
            }

            @Override
            public void beforeUndoTransparentActionFinished() {
                CommandListener.super.beforeUndoTransparentActionFinished();
            }

            @Override
            public void undoTransparentActionFinished() {
                CommandListener.super.undoTransparentActionFinished();
            }
        };

        BackAction goBackAction;

        new IdeDocumentHistoryImpl.RecentPlacesListener() {
            @Override
            public void recentPlaceAdded(IdeDocumentHistoryImpl.@NotNull PlaceInfo changePlace, boolean isChanged) {

            }

            @Override
            public void recentPlaceRemoved(IdeDocumentHistoryImpl.@NotNull PlaceInfo changePlace, boolean isChanged) {

            }
        };

        new FileDocumentManagerListener() {
            @Override
            public void beforeAllDocumentsSaving() {
                FileDocumentManagerListener.super.beforeAllDocumentsSaving();
            }

            @Override
            public void beforeDocumentSaving(@NotNull Document document) {
                FileDocumentManagerListener.super.beforeDocumentSaving(document);
            }

            @Override
            public void beforeFileContentReload(@NotNull VirtualFile file, @NotNull Document document) {
                FileDocumentManagerListener.super.beforeFileContentReload(file, document);
            }

            @Override
            public void fileWithNoDocumentChanged(@NotNull VirtualFile file) {
                FileDocumentManagerListener.super.fileWithNoDocumentChanged(file);
            }

            @Override
            public void fileContentReloaded(@NotNull VirtualFile file, @NotNull Document document) {
                FileDocumentManagerListener.super.fileContentReloaded(file, document);
            }

            @Override
            public void fileContentLoaded(@NotNull VirtualFile file, @NotNull Document document) {
                FileDocumentManagerListener.super.fileContentLoaded(file, document);
            }

            @Override
            public void unsavedDocumentDropped(@NotNull Document document) {
                FileDocumentManagerListener.super.unsavedDocumentDropped(document);
            }

            @Override
            public void unsavedDocumentsDropped() {
                FileDocumentManagerListener.super.unsavedDocumentsDropped();
            }

            @Override
            public void afterDocumentUnbound(@NotNull VirtualFile file, @NotNull Document document) {
                FileDocumentManagerListener.super.afterDocumentUnbound(file, document);
            }
        };

        DaemonListeners.class.toString();

        new DaemonCodeAnalyzer.DaemonListener() {
            @Override
            public void daemonStarting(@NotNull Collection<? extends FileEditor> fileEditors) {
                DaemonCodeAnalyzer.DaemonListener.super.daemonStarting(fileEditors);
            }

            @Override
            public void daemonFinished() {
                DaemonCodeAnalyzer.DaemonListener.super.daemonFinished();
            }

            @Override
            public void daemonFinished(@NotNull Collection<? extends FileEditor> fileEditors) {
                DaemonCodeAnalyzer.DaemonListener.super.daemonFinished(fileEditors);
            }

            @Override
            public void daemonCancelEventOccurred(@NotNull String reason) {
                DaemonCodeAnalyzer.DaemonListener.super.daemonCancelEventOccurred(reason);
            }

            @Override
            public void daemonAnnotatorStatisticsGenerated(@NotNull AnnotationSession session, @NotNull Collection<? extends AnnotatorStatistics> statistics, @NotNull PsiFile file) {
                DaemonCodeAnalyzer.DaemonListener.super.daemonAnnotatorStatisticsGenerated(session, statistics, file);
            }
        };

        // TODO: https://plugins.jetbrains.com/docs/intellij/reference-contributor.html
    }
}
