package com.jbalint.bs.bookstore;

import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;

import com.complexible.common.openrdf.util.ModelBuilder;
import com.complexible.common.openrdf.util.ResourceBuilder;
import com.complexible.common.rdf.model.StardogValueFactory;
import com.complexible.stardog.api.Connection;
import com.complexible.stardog.api.ConnectionConfiguration;
import com.jbalint.bs.ontology.BsLib;
import com.jbalint.bs.ontology.Nepomuk;
import org.apache.commons.codec.digest.DigestUtils;
import org.openrdf.model.IRI;
import org.openrdf.model.Model;

/**
 * Generate metadata structures for Bookstore db.
 */
public class FileMetadataGenerator {
	static final StardogValueFactory VF = StardogValueFactory.instance();

	Connection conn;
	Path root;
	IRI context;

	public FileMetadataGenerator(Connection conn, Path root, IRI context) {
		this.conn = conn;
		this.root = root;
		this.context = context;
	}

	/**
	 * Does an entry for the corresponding path exist in the DB?
	 */
	boolean existsInDb(Path p) {
		return conn.get().context(context).predicate(Nepomuk.NIE.url).object(getIRI(p)).statements().findFirst().isPresent();
	}

	/**
	 * Turn a Path into a model if it doesn't already exist in the DB and it's a regular file
	 */
	Model file(Path p) {
		try {
			Files.getLastModifiedTime(p);
			ResourceBuilder mainFile = new ModelBuilder()
				                           .instance(Nepomuk.NFO.RemoteDataObject)
				                           .addProperty(Nepomuk.NIE.url, getIRI(p))
				                           .addProperty(BsLib.relativePath, root.relativize(p).toString())
				                           .addProperty(Nepomuk.NIE.byteSize, Files.size(p))
				                           .addProperty(Nepomuk.NIE.lastRefreshed, new Date())
				                           .addProperty(Nepomuk.NFO.fileCreated, new Date(Files.getLastModifiedTime(p).toMillis()))
				                           .addProperty(Nepomuk.NFO.fileName, p.getFileName().toString())
				                           .addProperty(Nepomuk.NFO.hasHash, new ModelBuilder().instance(Nepomuk.NFO.FileHash)
				                                                                               .addProperty(Nepomuk.NFO.hashAlgorithm, "MD5")
				                                                                               .addProperty(Nepomuk.NFO.hashValue,
				                                                                                            DigestUtils.md5Hex(Files.newInputStream(p))));
			ResourceBuilder infoElement = new ModelBuilder()
				                              .instance(Nepomuk.NIE.InformationElement)
				                              .addProperty(StardogValueFactory.RDF.TYPE, BsLib.Book)
				                              .addProperty(Nepomuk.NIE.isStoredAs, mainFile);

			return infoElement.model();
		}
		catch (IOException theE) {
			throw new RuntimeException(theE);
		}
	}

	void addToDb(Model m) {
		conn.begin();
		conn.add().graph(m, context);
		conn.commit();
	}

	private IRI getIRI(final Path p) {
		try {
			// URL escaping, per http://stackoverflow.com/a/25735202/1090617
			// was using URLEncoder.encode() but that's form HTTP query parameters (i.e. uses "+" for space instead of "%20")
			String urlStr = "https://localhost/bookstore/" + root.relativize(p).toString();
			URL url= new URL(urlStr);
			URI uri = new URI(url.getProtocol(), url.getUserInfo(), url.getHost(), url.getPort(), url.getPath(), url.getQuery(), url.getRef());
			return VF.createIRI(uri.toASCIIString());
		}
		catch (Exception theE) {
			throw new RuntimeException(theE);
		}
	}

	public static void main(final String args[]) throws Exception {
		if (args.length != 3) {
			System.err.println("Usage: FileMetadataGenerator <db-url> <fileroot> <graph-iri>");
			System.exit(1);
		}

		FileMetadataGenerator fmg = new FileMetadataGenerator(ConnectionConfiguration.at(args[0]),
		                                                      Paths.get(args[1]),
		                                                      VF.createIRI(args[2]));

		long booksAdded;
		boolean TESTING = false;
		if (TESTING) {
			booksAdded = Files.walk(fmg.root)
			                       .filter(Files::isRegularFile)
			                       .limit(1)
			                       .map(fmg::file)
			                       .count();
		} else {
			booksAdded = Files.walk(fmg.root)
			                       .filter(Files::isRegularFile)
			                       .filter(f -> !fmg.existsInDb(f))
			                       .map(fmg::file)
			                       .peek(fmg::addToDb)
			                       .count();
		}

		System.err.println("Books added: " + booksAdded);

		//StatementSources.write(MemoryStatementSource.of(fmg.statements), RDFFormat.TURTLE, new FileOutputStream("/tmp/books.ttl"));
	}
}
