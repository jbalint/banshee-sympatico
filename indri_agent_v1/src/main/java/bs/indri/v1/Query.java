package bs.indri.v1;

import java.util.Map;
import java.util.Set;

public class Query {
	private int queryId;
	private String queryString;

	public Query(Set<String> indexPaths, String query) {
		this.queryId = query_begin(indexPaths.toArray(new String[] {}), query);
		this.queryString = query;
	}

	public Map<String, Object> nextResults(int resultCount, Set<String> fields) {
		return query_next_results(this.queryId, resultCount,
								  fields.toArray(new String[] {}));
	}

	public void close() {
		query_close(this.queryId);
	}

	static {
		System.loadLibrary("bs_indri_v1_Query");
	}
	private native int query_begin(String indexPaths[], String query);
	private native Map<String, Object> query_next_results(int queryId, int resultCount,
														  String fields[]);
	private native void query_close(int queryId);
}
