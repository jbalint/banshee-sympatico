package bs.wmii.v1.ontology;

public class SubscribeRequest implements jade.content.AgentAction {
	public static final int REQUEST_TYPE_SUBSCRIBE = 1;
	public static final int REQUEST_TYPE_UNSUBSCRIBE = 2;

	private int requestType;

	public SubscribeRequest() {
	}

	public SubscribeRequest(int x) {
		this.requestType = x;
	}

	public void setRequestType(int x) {
		this.requestType = x;
	}

	public int getRequestType() {
		return this.requestType;
	}
}
