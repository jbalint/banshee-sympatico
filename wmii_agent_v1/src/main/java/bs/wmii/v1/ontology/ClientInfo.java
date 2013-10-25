package bs.wmii.v1.ontology;

import java.util.HashSet;
import java.util.Set;

public class ClientInfo implements jade.content.Concept {
	private String id;
	private String name;
	private String windowClass;
	private String label;
	private Set<String> tags = new HashSet<String>();
	
	public String getId() { return id; }
	
	public void setId(String id) {
	    this.id = id;
	}
	
	public String getName() { return name; }
	
	public void setName(String name) {
	    this.name = name;
	}
	
	public String getWindowClass() { return windowClass; }
	
	public void setWindowClass(String windowClass) {
	    this.windowClass = windowClass;
	}
	
	public String getLabel() { return label; }
	
	public void setLabel(String label) {
	    this.label = label;
	}

	public String toString() {
		return "Client: " + getId();
	}

	public boolean equals(Object o) {
		if (o == null ||
			getId() == null ||
			!ClientInfo.class.isAssignableFrom(o.getClass()))
			return false;

		ClientInfo ci = (ClientInfo) o;
		return ci.getId().equals(ci.getId());
	}
}
