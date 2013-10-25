package bs.wmii.v1;

import jade.core.Agent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import bs.wmii.v1.ontology.WmiiOntology;

public class WmiiAgent extends jade.core.Agent {
	private Logger log;

	private SubscriptionManager submgr;

	private jade.content.lang.Codec codec = new jade.content.lang.sl.SLCodec();

	public WmiiAgent() {
		submgr = new SubscriptionManager(this);
	}

	protected void setup() {
		log = LoggerFactory.getLogger(getName());

		getContentManager().registerLanguage(codec);
		getContentManager().registerOntology(WmiiOntology.getInstance());
		addBehaviour(submgr);
	}
}
