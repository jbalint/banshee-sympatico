package com.jbalint.bs.bookstore;

import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;
import java.util.Set;

import com.complexible.stardog.api.Connection;
import com.complexible.stardog.api.ConnectionConfiguration;
import com.stardog.stark.IRI;
import com.stardog.stark.Statement;
import com.stardog.stark.Values;
import com.stardog.stark.util.GraphBuilder;
import com.stardog.stark.util.ResourceBuilder;

import com.jbalint.bs.ontology.BsLib;
import com.jbalint.bs.ontology.Nepomuk;
import org.apache.commons.codec.digest.DigestUtils;

/**
 * Generate metadata structures for Bookstore db.
 */
public class FileMetadataGenerator {

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
		return conn.get().context(context).predicate(Nepomuk.NIE.url).object(getIRI(p)).ask();
	}

	/**
	 * Turn a Path into a model if it doesn't already exist in the DB and it's a regular file
	 */
	Set<Statement> file(Path p) {
		try {
			Files.getLastModifiedTime(p);
			ResourceBuilder mainFile = new GraphBuilder()
				                           .instance(Nepomuk.NFO.RemoteDataObject)
				                           .addProperty(Nepomuk.NIE.url, getIRI(p))
				                           .addProperty(BsLib.relativePath, root.relativize(p).toString())
				                           .addProperty(Nepomuk.NIE.byteSize, Files.size(p))
				                           .addProperty(Nepomuk.NIE.lastRefreshed, new Date())
				                           .addProperty(Nepomuk.NFO.fileCreated, new Date(Files.getLastModifiedTime(p).toMillis()))
				                           .addProperty(Nepomuk.NFO.fileName, p.getFileName().toString())
				                           .addProperty(Nepomuk.NFO.hasHash, new GraphBuilder().instance(Nepomuk.NFO.FileHash)
				                                                                               .addProperty(Nepomuk.NFO.hashAlgorithm, "MD5")
				                                                                               .addProperty(Nepomuk.NFO.hashValue,
				                                                                                            DigestUtils.md5Hex(Files.newInputStream(p))));
			ResourceBuilder infoElement = new GraphBuilder()
				                              .instance(Nepomuk.NIE.InformationElement)
				                              .addProperty(com.stardog.stark.vocabs.RDF.TYPE, BsLib.Book)
				                              .addProperty(Nepomuk.NIE.isStoredAs, mainFile);

			return infoElement.graph();
		}
		catch (IOException theE) {
			throw new RuntimeException(theE);
		}
	}

	void addToDb(Set<Statement> m) {
		conn.begin();
		conn.add().graph(m, context);
		conn.commit();
	}

	private IRI getIRI(final Path p) {
		try {
			// URL escaping, per http://stackoverflow.com/a/25735202/1090617
			// was using URLEncoder.encode() but that's form HTTP query parameters (i.e. uses "+" for space instead of "%20")
			String urlStr = "https://localhost/bookstore/" + root.relativize(p).toString();
			URL url = new URL(urlStr);
			URI uri = new URI(url.getProtocol(), url.getUserInfo(), url.getHost(), url.getPort(), url.getPath(), url.getQuery(), url.getRef());
			return Values.iri(uri.toASCIIString());
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
		                                                      Values.iri(args[2]));

		long booksAdded;
		boolean TESTING = false;
		if (TESTING) {
			booksAdded = Files.walk(fmg.root)
			                  .filter(Files::isRegularFile)
			                  .limit(1)
			                  .map(fmg::file)
			                  .peek(System.out::println)
			                  .count();
		}
		else {
			booksAdded = Files.walk(fmg.root)
			                  .filter(Files::isRegularFile)
			                  .filter(f -> !fmg.existsInDb(f))
			                  .map(fmg::file)
			                  .peek(System.out::println)
			                  .peek(fmg::addToDb)
			                  .count();
		}

		System.err.println("Books added: " + booksAdded);

		//StatementSources.write(MemoryStatementSource.of(fmg.statements), RDFFormat.TURTLE, new FileOutputStream("/tmp/books.ttl"));
	}
}
