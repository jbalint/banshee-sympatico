package bs.wmii.v1;

import java.util.HashSet;
import java.util.Set;

import jade.content.onto.basic.Action;
import jade.core.Agent;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.proto.states.MsgReceiver;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SubscriptionManager extends MsgReceiver {
	private static final Logger log = LoggerFactory.getLogger(SubscriptionManager.class);

	private static final MessageTemplate mt =
		MessageTemplate.and(MessageTemplate.MatchPerformative(ACLMessage.SUBSCRIBE),
							MessageTemplate.MatchOntology(WmiiOntology.ONTOLOGY_NAME));

	private Set<AID> subscribers = new HashSet<AID>();;

	public SubscriptionManager(Agent agent) {
		super(agent, mt,
			  /* deadline */ MsgReceiver.INFINITE,
			  /* datastore */ null,
			  /* msgkey */ null);
	}

	protected void handleMessage(ACLMessage msg) {
		SubscribeRequest req;
		AID sender = msg.getSender();

		// unmarshal request
		try {
			req = (SubscribeRequest) ((Action) getAgent().getContentManager().extractContent(msg)).getAction();
		} catch(Exception ex) {
			log.error("Cannot extract content from message: " + msg, ex);
		}

		// process request
		if (req.getRequestType() == SubscribeRequest.REQUEST_TYPE_SUBSCRIBE)
			subscribers.add(sender);
		else
			subscribers.remove(sender);

		// confirm request processing
		try {
			ACLMessage reply = msg.createReply();
			reply.setPerformative(ACLMessage.CONFIRM);
			getAgent().getContentManager().fillContent(reply, req);
			getAgent().send(reply);
		} catch(Exception ex) {
			log.error("Failed to send sub/unsub confirmation to " + sender +
					  ". Removing from subscribers list.", ex);
			subscribers.remove(sender);
		}
	}

	public boolean done() {
		return false;
	}
}
