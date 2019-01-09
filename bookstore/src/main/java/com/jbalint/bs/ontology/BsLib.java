package com.jbalint.bs.ontology;

import com.stardog.stark.IRI;
import com.stardog.stark.Values;

/**
 * Created by jbalint on 1/16/17.
 */
public class BsLib {
	private static final String NS = "http://banshee-sympatico/lib#";

	public static final IRI Book = Values.iri(NS, "Book");

	public static final IRI relativePath = Values.iri(NS, "relativePath");
	public static final IRI hasBibtex = Values.iri(NS, "hasBibtex");
}
