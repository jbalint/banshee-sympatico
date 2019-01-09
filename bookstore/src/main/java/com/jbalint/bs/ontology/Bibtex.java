package com.jbalint.bs.ontology;

import com.stardog.stark.IRI;
import com.stardog.stark.Values;

/**
 * Created by jbalint on 2/20/17.
 */
public class Bibtex {
		private static final String NS = "http://purl.org/net/nknouf/ns/bibtex#";

	public static final IRI Author = Values.iri(NS, "Author");
	public static final IRI Misc = Values.iri(NS, "Misc");

	public static final IRI hasAbstract = Values.iri(NS, "hasAbstract");
	public static final IRI hasTitle = Values.iri(NS, "hasTitle");
}
