package jbalint.sythesis;

import com.google.common.collect.Maps;
import com.intellij.lang.java.JavaParserDefinition;
import com.intellij.psi.impl.cache.impl.todo.TodoIndexEntry;
import com.intellij.util.indexing.*;
import com.intellij.util.io.DataExternalizer;
import com.intellij.util.io.KeyDescriptor;
import org.jetbrains.annotations.NotNull;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.Map;

public class Indexer extends FileBasedIndexExtension<String, String> {
    public Indexer() {
        super();
    }

    @Override
    public @NotNull ID<String, String> getName() {
        return ID.create("AnalysisIndexer");
    }

    @Override
    public @NotNull DataIndexer<String, String, FileContent> getIndexer() {
        return new DataIndexer<String, String, FileContent>() {
            @Override
            public @NotNull Map<String, String> map(@NotNull FileContent inputData) {
                return Maps.newHashMap();
            }
        };
    }

    @Override
    public @NotNull KeyDescriptor<String> getKeyDescriptor() {
        // TODO : copied from TodoIndex
        return new KeyDescriptor<String>() {
            @Override
            public int getHashCode(final String value) {
                return value.hashCode();
            }

            @Override
            public boolean isEqual(final String val1, final String val2) {
                return val1.equals(val2);
            }

            @Override
            public void save(@NotNull final DataOutput out, final String value) throws IOException {
//                out.writeUTF(value.pattern);
//                out.writeBoolean(value.caseSensitive);
            }

            @Override
            public String read(@NotNull final DataInput in) throws IOException {
                return "";
//                throw new UnsupportedOperationException();
            }
        };
    }

    @Override
    public @NotNull DataExternalizer<String> getValueExternalizer() {
        return new DataExternalizer<String>() {
            @Override
            public void save(@NotNull DataOutput out, String value) throws IOException {

            }

            @Override
            public String read(@NotNull DataInput in) throws IOException {
                return "null";
            }
        };
    }

    @Override
    public int getVersion() {
        return 0;
    }

    @Override
    public FileBasedIndex.@NotNull InputFilter getInputFilter() {
        return JavaParserDefinition.JAVA_FILE::shouldBuildStubFor;
    }

    @Override
    public boolean dependsOnFileContent() {
        return true;
    }
}
