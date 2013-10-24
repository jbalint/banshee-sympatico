package bs.wmii.v1.ontology;

import jade.content.onto.BeanOntology;
import jade.content.onto.BeanOntologyException;

public class WmiiOntology extends BeanOntology {
	public static final String ONTOLOGY_NAME = "Wmii-v1-Ontology";

	private static final WmiiOntology instance = new WmiiOntology();

	public static WmiiOntology getInstance() {
		return instance;
	}

	private WmiiOntology() {
		super(ONTOLOGY_NAME);

		try {
			add("bs.wmii.v1.ontology");
		} catch(BeanOntologyException ex) {
			throw new RuntimeException("error building ontology", ex);
		}
	}
}
