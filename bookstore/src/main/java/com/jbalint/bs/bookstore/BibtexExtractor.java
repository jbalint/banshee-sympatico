package com.jbalint.bs.bookstore;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.UUID;
import java.util.stream.Collectors;

import be.ugent.mmlab.rml.core.RMLEngine;
import be.ugent.mmlab.rml.core.StdRMLEngine;
import be.ugent.mmlab.rml.mapdochandler.extraction.std.StdRMLMappingFactory;
import be.ugent.mmlab.rml.mapdochandler.retrieval.RMLDocRetrieval;
import be.ugent.mmlab.rml.model.RMLMapping;
import com.complexible.common.openrdf.model.ModelIO;
import com.complexible.common.openrdf.model.Models2;
import com.complexible.common.rdf.model.StardogValueFactory;
import com.complexible.common.rdf.model.Values;
import com.complexible.stardog.api.Connection;
import com.complexible.stardog.api.ConnectionConfiguration;
import com.jbalint.bs.ontology.Bibtex;
import com.jbalint.bs.ontology.BsLib;
import org.grobid.core.data.BiblioItem;
import org.grobid.core.engines.Engine;
import org.grobid.core.factory.GrobidFactory;
import org.grobid.core.mock.MockContext;
import org.openrdf.model.Model;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.query.BindingSet;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.repository.Repository;
import org.openrdf.rio.RDFFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Extract BibTeX entries using GROBID.
 *
 * TODO : lots of constants here need to be parameterized
 */
public class BibtexExtractor {
	private static final Logger LOGGER = LoggerFactory.getLogger(BibtexExtractor.class);

	private static final String GROBID_HOME = "/home/jbalint/sw/java-sw/grobid/grobid-home";

	private Engine mGrobid;

	RMLEngine engine = new StdRMLEngine("/tmp/rml-grobid-output.ttl");

	public BibtexExtractor() {
		mGrobid = GrobidFactory.getInstance().createEngine();
	}

	private static void init() {
		// this calls some JNDI stuff and can't be called twice
		try {
			MockContext.setInitialContext(GROBID_HOME,
			                              GROBID_HOME + "/config/grobid.properties");
		} catch (Exception e) {
			LOGGER.error("Failed to initialize GROBID", e);
		}
	}

	/**
	 * Run the GROBID extractor on the file
	 */
	String extractTei(String path) {
		BiblioItem biblioItem = new BiblioItem();
		return mGrobid.processHeader(path, false, biblioItem);
	}

	/**
	 * Convert the TEI XML string to RDF
	 *
	 * NOTE : this is NOT thread-safe due to use of a single set of temp files
	 */
	void xmlToRdf(String theTei) {
		String aTemplate = "src/main/resources/grobid-bibtex-mapping.ttl";
		String aTempFilename = "/tmp/rml-grobid-mapping-temp.ttl";
		String aTempXmlFile = "/tmp/rml-grobid-input.xml"; // has to agree with filename in mapping

		try {
			// write XML to a file (need to use lower level RML-Mapper APIs to avoid this)
			// (Need to get rid of xmlns for the XPath to work properly)
			theTei = theTei.replaceAll("xmlns=\"http://www.tei-c.org/ns/1.0\"", "");
			Files.write(Paths.get(aTempXmlFile),
			            theTei.getBytes("UTF-8"));

			// create a new mapping for EACH doc, using a UUID generated externally
			String aUuid = UUID.randomUUID().toString();
			String newMapping = Files.readAllLines(Paths.get(aTemplate))
			                         .stream()
			                         .collect(Collectors.joining("\n"))
			                         .replaceAll("GENERATED-UUID", aUuid);
			Files.write(Paths.get(aTempFilename), newMapping.getBytes("UTF-8"));
		}
		catch (IOException theE) {
			throw new RuntimeException(theE);
		}

		// read mappings
		StdRMLMappingFactory mappingFactory = new StdRMLMappingFactory();
		RMLDocRetrieval mapDocRetrieval = new RMLDocRetrieval();
		Repository repository =
			mapDocRetrieval.getMappingDoc(aTempFilename, RDFFormat.TURTLE);
		RMLMapping mapping = mappingFactory.extractRMLMapping(repository);

		// run mapping engine (this arg to Engine is used as the DIR for Sesame root)
		engine.run(mapping, "/tmp/rml-grobid-output.ttl", "turtle",
		           "", null, null,
		           null, null, null);
	}

	public static void main(String args[]) throws Exception {
		if (args.length != 3) {
			System.err.println("Usage: FileMetadataGenerator <db-url> <fileroot> <graph-iri>");
			System.exit(1);
		}
		String aDbUrl = args[0];
		String aFileroot = args[1];
		String aBslibGraph = args[2];

		Connection aConn = ConnectionConfiguration.at(aDbUrl);
		aConn.begin();
		String aQuery = "select * from <BSLIB-GRAPH> { ?book a bslib:Book ; nie:isStoredAs/bslib:relativePath ?path }";
		aQuery = aQuery.replaceAll("BSLIB-GRAPH", aBslibGraph);
		TupleQueryResult aResult = aConn.select(aQuery).execute();

		init();
		BibtexExtractor aExtractor = new BibtexExtractor();

		while (aResult.hasNext()) {
			BindingSet aBindings = aResult.next();
			Resource aBook = (Resource) aBindings.getValue("book");
			String aPath = aBindings.getValue("path").stringValue();
			if (!aPath.endsWith(".pdf")) {
				continue;
			}
			System.err.println("Processing: " + aPath);
			String aTei = null;
			try {
				aTei = aExtractor.extractTei(aFileroot + "/" + aPath);
			}
			catch (Exception theE) {
				LOGGER.error("Failed to extract: " + aPath, theE);
				continue;
			}
			try {
				aExtractor.xmlToRdf(aTei);
			}
			catch (Exception theE) {
				LOGGER.error("Failed to map RDF: " + aPath, theE);
				continue;
			}
			Model aStatements = ModelIO.read(Paths.get("/tmp/rml-grobid-output.ttl"), RDFFormat.TURTLE);
			Statement aBibtex = aStatements.filter(null, StardogValueFactory.RDF.TYPE, Bibtex.Misc).stream().findFirst().orElse(null);
			if (aBibtex != null) {
				Model aBibtexRef = Models2.newModel(Values.statement(aBook, BsLib.hasBibtex, aBibtex.getSubject()));
				aConn.add().graph(aBibtexRef, Values.iri(aBslibGraph));
			}
			aConn.add().graph(aStatements, Values.iri("http://banshee-sympatico/bookstore/grobid-extract-2017-02-20"));
		}
		aConn.commit();
		aConn.close();
	}
}
