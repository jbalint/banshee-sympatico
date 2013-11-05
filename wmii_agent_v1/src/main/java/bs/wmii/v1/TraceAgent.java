package bs.wmii.v1;

import jade.core.Agent;
import jade.core.AID;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import bs.jade.MessageReceiver;
import bs.jade.MessageReceiverFacade;
import bs.wmii.v1.ontology.SubscribeRequest;
import bs.wmii.v1.ontology.WmiiOntology;

public class TraceAgent extends Agent implements MessageReceiver {
	private Logger log;

	private jade.content.lang.Codec codec = new jade.content.lang.sl.SLCodec();

	public TraceAgent() {
	}

	private void subscribeToEvents() {
		log = LoggerFactory.getLogger(getName());

		getContentManager().registerLanguage(codec);
		getContentManager().registerOntology(WmiiOntology.getInstance());

		SubscribeRequest request = new SubscribeRequest(SubscribeRequest.REQUEST_TYPE_SUBSCRIBE);

		ACLMessage m = new ACLMessage(ACLMessage.SUBSCRIBE);
		AID aid = new AID("wmii", AID.ISLOCALNAME);
		try {
			m.setOntology(WmiiOntology.ONTOLOGY_NAME);
			m.setLanguage(codec.getName());
			getContentManager().fillContent(m, new jade.content.onto.basic.Action(aid, request));
		} catch(Exception ex) {
			log.error("Failed to create subscribe message", ex);
			return;
		}
		m.addReceiver(aid);
		send(m);
	}

	protected void setup() {
		MessageTemplate informOrConfirm =
			MessageTemplate.or(MessageTemplate.MatchPerformative(ACLMessage.INFORM),
							   MessageTemplate.MatchPerformative(ACLMessage.CONFIRM));
		MessageTemplate mt =
			MessageTemplate.and(informOrConfirm,
								MessageTemplate.MatchOntology(WmiiOntology.ONTOLOGY_NAME));
		addBehaviour(new MessageReceiverFacade(this, this, mt));
		subscribeToEvents();
	}

	public void handleMessage(ACLMessage msg) {
		if (msg.getPerformative() == ACLMessage.CONFIRM) {
			log.info("wmii trace subscription confirmed");
			System.out.println(msg);
			return;
		}
		// TODO
		System.out.println(msg);
	}

	public boolean done() {
		return false;
	}
}
