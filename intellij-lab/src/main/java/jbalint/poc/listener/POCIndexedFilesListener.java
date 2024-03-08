package jbalint.poc.listener;

import com.intellij.openapi.roots.ContentIterator;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.indexing.events.IndexedFilesListener;
import org.jetbrains.annotations.NotNull;

public class POCIndexedFilesListener extends IndexedFilesListener {
    @Override
    protected void iterateIndexableFiles(@NotNull VirtualFile file, @NotNull ContentIterator iterator) {

    }
}
