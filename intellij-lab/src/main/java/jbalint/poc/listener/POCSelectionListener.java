package jbalint.poc.listener;

import com.intellij.openapi.editor.event.SelectionEvent;
import com.intellij.openapi.editor.event.SelectionListener;
import org.jetbrains.annotations.NotNull;

public class POCSelectionListener implements SelectionListener {
    @Override
    public void selectionChanged(@NotNull SelectionEvent e) {
        SelectionListener.super.selectionChanged(e);
    }
}
