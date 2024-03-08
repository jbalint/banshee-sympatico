package jbalint.poc.listener;

import com.intellij.ide.bookmark.Bookmark;
import com.intellij.ide.bookmark.BookmarkGroup;
import com.intellij.ide.bookmark.BookmarksListener;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class POCBookmarksListener
    // I *think* this is the newer version of the interface
        implements BookmarksListener {
    @Override
    public void groupsSorted() {
        BookmarksListener.super.groupsSorted();
    }

    @Override
    public void groupAdded(@NotNull BookmarkGroup group) {
        BookmarksListener.super.groupAdded(group);
    }

    @Override
    public void groupRemoved(@NotNull BookmarkGroup group) {
        BookmarksListener.super.groupRemoved(group);
    }

    @Override
    public void groupRenamed(@NotNull BookmarkGroup group) {
        BookmarksListener.super.groupRenamed(group);
    }

    @Override
    public void bookmarksSorted(@NotNull BookmarkGroup group) {
        BookmarksListener.super.bookmarksSorted(group);
    }

    @Override
    public void bookmarkAdded(@NotNull BookmarkGroup group, @NotNull Bookmark bookmark) {
        BookmarksListener.super.bookmarkAdded(group, bookmark);
    }

    @Override
    public void bookmarkRemoved(@NotNull BookmarkGroup group, @NotNull Bookmark bookmark) {
        BookmarksListener.super.bookmarkRemoved(group, bookmark);
    }

    @Override
    public void bookmarkChanged(@NotNull BookmarkGroup group, @NotNull Bookmark bookmark) {
        BookmarksListener.super.bookmarkChanged(group, bookmark);
    }

    @Override
    public void bookmarkTypeChanged(@NotNull Bookmark bookmark) {
        BookmarksListener.super.bookmarkTypeChanged(bookmark);
    }

    @Override
    public void defaultGroupChanged(@Nullable BookmarkGroup oldGroup, @Nullable BookmarkGroup newGroup) {
        BookmarksListener.super.defaultGroupChanged(oldGroup, newGroup);
    }

    @Override
    public void structureChanged(@Nullable Object node) {
        BookmarksListener.super.structureChanged(node);
    }
}
