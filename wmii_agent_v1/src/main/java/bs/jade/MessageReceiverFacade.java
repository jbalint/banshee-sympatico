package bs.jade;

import jade.core.Agent;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;

public class MessageReceiverFacade extends jade.proto.states.MsgReceiver {
	private MessageReceiver receiver;

	public MessageReceiverFacade(Agent agent, MessageReceiver receiver, MessageTemplate mt) {
		super(agent, mt, INFINITE, null, null);
		this.receiver = receiver;
	}

	protected void handleMessage(ACLMessage msg) {
		receiver.handleMessage(msg);
	}

	public boolean done() {
		return receiver.done();
	}
}
