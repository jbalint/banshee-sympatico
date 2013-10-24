package bs.wmii.v1.ontology;

public class ClientEvent implements jade.content.Concept {
	public static final int EVENT_TYPE_FOCUS = 1;
	public static final int EVENT_TYPE_CREATED = 2;
	public static final int EVENT_TYPE_DESTROYED = 3;

	private int eventType;
	private String clientId;

	public ClientEvent(int eventType, String clientId) {
		this.eventType = eventType;
		this.clientId = clientId;
	}

	public void setEventType(int eventType) {
		this.eventType = eventType;
	}

	public int getEventType() {
		return this.eventType;
	}

	public void setClientId(String clientId) {
		this.clientId = clientId;
	}

	public String getClientId() {
		return this.clientId;
	}
}
