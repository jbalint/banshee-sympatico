package bs.jade;

import jade.lang.acl.ACLMessage;

public interface MessageReceiver {
	public void handleMessage(ACLMessage msg);
	public boolean done();
}
