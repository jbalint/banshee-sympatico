package bs.wmii.v1;

import java.io.*;
import java.util.*;

/**
 * Thin wrapper around `wmiir'. Comes from rev a26834ec1ab1a20060e5840c2ada9abca3250682
 * in github.com/jbalint/desktop_agents_jade.
 */
public class Wmiir {
	private static String readFully(InputStream is) throws IOException {
		StringBuilder b = new StringBuilder();
		InputStreamReader r = new InputStreamReader(is);
		char buf[] = new char[20];
		int n;
		while ((n = r.read(buf, 0, 20)) > 0) {
			b.append(buf, 0, n);
		}
		r.close();
		return b.toString();
	}

	private static String getCommandOutput(ProcessBuilder pb) throws IOException {
		Process p = pb.start();
		String commandOutput = readFully(p.getInputStream());
		p.destroy();
		return commandOutput;
	}

	private static String getPipedCommandOutput(ProcessBuilder cmd, ProcessBuilder grep) throws IOException {
		Process cmdProc = cmd.start();
		Process grepProc = grep.start();
		BufferedReader mainReader = new BufferedReader(new InputStreamReader(cmdProc.getInputStream()));
		BufferedWriter grepWriter = new BufferedWriter(new OutputStreamWriter(grepProc.getOutputStream()));
		BufferedReader grepReader = new BufferedReader(new InputStreamReader(grepProc.getInputStream()));
		String s;
		StringBuilder result = new StringBuilder();
		char buf[] = new char[1024];
		int n;
		while ((s = mainReader.readLine()) != null) {
			grepWriter.write(s + "\n");
			if (grepReader.ready()) {
				if ((n = grepReader.read(buf, 0, 1024)) > 0)
					result.append(buf, 0, n);
			}
		}
		grepWriter.close();
		while ((n = grepReader.read(buf, 0, 1024)) > 0) {
			result.append(buf, 0, n);
		}
		return result.toString();
	}

	public static Set<String> getTagNames() throws IOException {
		Set<String> tagNames = new HashSet<>();

		Process p = new ProcessBuilder("wmiir", "ls", "/tag").start();
		BufferedReader r = new BufferedReader(new InputStreamReader(p.getInputStream()));
		String tagDir;
		while ((tagDir = r.readLine()) != null) {
			if ("sel/".equals(tagDir)) {
				continue;
			}
			tagNames.add(tagDir.replaceFirst("/$", ""));
		}
		p.destroy();
		r.close();
		return tagNames;
	}

	public static String getClientLabel(String clientId) throws IOException {
		return getCommandOutput(new ProcessBuilder("wmiir", "read", "/client/" + clientId + "/label"));
	}

	public static Set<String> getClientTagNames(String clientId) throws IOException {
		String tagNames = getCommandOutput(new ProcessBuilder("wmiir", "read", "/client/" + clientId + "/tags"));
		Set<String> tagNameSet = new HashSet<>();
		for (String s : tagNames.split("\\+")) {
			tagNameSet.add(s);
		}
		return tagNameSet;
	}

	public static String getCurrentTagName() throws IOException {
		String o = getPipedCommandOutput(new ProcessBuilder("wmiir", "read", "/ctl"),
										 new ProcessBuilder("grep", "^view"));
		return o.replaceAll("view\\s+(.+)\\n", "$1");
	}
}
