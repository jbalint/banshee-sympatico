package bs.wmii.v1.ontology;

public class ClientInfoRequest extends jade.content.onto.basic.Action {
	private String clientId;

	public ClientInfoRequest() {
	}

	public ClientInfoRequest(String clientId) {
		setClientId(clientId);
	}

	public String getClientId() {
		return this.clientId;
	}

	public void setClientId(String clientId) {
		this.clientId = clientId;
	}
}
