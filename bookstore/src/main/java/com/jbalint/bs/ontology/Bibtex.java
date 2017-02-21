package com.jbalint.bs.ontology;

import com.complexible.common.openrdf.vocabulary.Vocabulary;
import org.openrdf.model.IRI;

/**
 * Created by jbalint on 2/20/17.
 */
public class Bibtex extends Vocabulary {
	private static final Bibtex INSTANCE = new Bibtex();

	private Bibtex() {
		super("http://purl.org/net/nknouf/ns/bibtex#");
	}

	public static final IRI Author = INSTANCE.term("Author");
	public static final IRI Misc = INSTANCE.term("Misc");

	public static final IRI hasAbstract = INSTANCE.term("hasAbstract");
	public static final IRI hasTitle = INSTANCE.term("hasTitle");
}
