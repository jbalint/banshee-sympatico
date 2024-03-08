package jbalint.poc.listener;

import com.intellij.openapi.vfs.newvfs.BulkFileListener;
import com.intellij.openapi.vfs.newvfs.events.VFileEvent;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class POCBulkFileListener implements BulkFileListener {
    @Override
    public void before(@NotNull List<? extends @NotNull VFileEvent> events) {
        BulkFileListener.super.before(events);
    }

    @Override
    public void after(@NotNull List<? extends @NotNull VFileEvent> events) {
        BulkFileListener.super.after(events);
    }
}
