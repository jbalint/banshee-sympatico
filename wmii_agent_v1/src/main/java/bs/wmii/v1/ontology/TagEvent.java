package bs.wmii.v1.ontology;

public class TagEvent implements jade.content.Concept {
	public static final int EVENT_TYPE_FOCUS = 1;
	public static final int EVENT_TYPE_CREATED = 2;
	public static final int EVENT_TYPE_DESTROYED = 3;
	public static final int EVENT_TYPE_URGENT = 4;

	private int eventType;
	private String tagId;

	public TagEvent() {
	}

	public TagEvent(int eventType, String tagId) {
		this.eventType = eventType;
		this.tagId = tagId;
	}

	public void setEventType(int eventType) {
		this.eventType = eventType;
	}

	public int getEventType() {
		return this.eventType;
	}

	public void setTagId(String tagId) {
		this.tagId = tagId;
	}

	public String getTagId() {
		return this.tagId;
	}

	public String toString() {
		String eventName;
		switch (this.eventType) {
		case EVENT_TYPE_FOCUS:
			eventName = "FocusTag";
			break;
		case EVENT_TYPE_CREATED:
			eventName = "CreateTag";
			break;
		case EVENT_TYPE_DESTROYED:
			eventName = "DestroyTag";
			break;
		default:
			eventName = "<unknown>";
			break;
		}
		return eventName + " " + this.tagId;
	}
}
