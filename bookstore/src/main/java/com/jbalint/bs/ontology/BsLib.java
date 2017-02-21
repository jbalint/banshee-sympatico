package com.jbalint.bs.ontology;

import com.complexible.common.openrdf.vocabulary.Vocabulary;
import org.openrdf.model.IRI;

/**
 * Created by jbalint on 1/16/17.
 */
public class BsLib extends Vocabulary {
	private static final BsLib INSTANCE = new BsLib();

	private BsLib() {
		super("http://banshee-sympatico/lib#");
	}

	public static final IRI Book = INSTANCE.term("Book");

	public static final IRI relativePath = INSTANCE.term("relativePath");
	public static final IRI hasBibtex = INSTANCE.term("hasBibtex");
}
