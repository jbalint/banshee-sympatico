package bs.indri.v1;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class TestQuery {
	public static void main(String args[]) {
		String queryString = "something OR filter";//"filter";
		Set indexPaths = new HashSet(Arrays.asList(new String[] {"/home/jbalint/sw/indri-5.5/my_first_index"}));
		Query q = new Query(indexPaths, queryString);
		Set fields = new HashSet(Arrays.asList(new String[] {"title", "date", "snippet", "content", "relevance"}));
		System.out.println("----------------------------------");
		System.out.println(q.nextResults(1, fields));
		System.out.println("----------------------------------");
		System.out.println(q.nextResults(1, fields));
	}
}
