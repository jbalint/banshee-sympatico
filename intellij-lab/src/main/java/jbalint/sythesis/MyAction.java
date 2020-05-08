package jbalint.sythesis;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import org.jetbrains.annotations.NotNull;

import java.util.Date;

/**
 * TODO : doc me
 * 1234
 */
public class MyAction extends AnAction {

    private static final Date LOAD_TIME = new Date();

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        System.err.println("The action has been performed on version: " + LOAD_TIME);
    }
}
