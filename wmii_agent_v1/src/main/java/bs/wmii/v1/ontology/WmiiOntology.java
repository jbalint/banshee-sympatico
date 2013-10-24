package bs.wmii.v1.ontology;

public class WmiiOntology extends BeanOntology {
	public static final String ONTOLOGY_NAME = "Wmii-v1-Ontology";

	private static final WmiiOntology instance = new WmiiOntology();

	public static WmiiOntology getInstance() {
		return instance;
	}

	private WmiiOntology() throws Exception {
		super(ONTOLOGY_NAME);

		add("bs.wmii.v1.ontology");
	}
}
