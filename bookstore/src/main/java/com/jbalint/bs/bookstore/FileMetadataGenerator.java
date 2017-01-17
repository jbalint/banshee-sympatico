package com.jbalint.bs.bookstore;

import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;
import java.util.Objects;

import com.complexible.common.openrdf.util.ModelBuilder;
import com.complexible.common.openrdf.util.ResourceBuilder;
import com.complexible.common.rdf.model.StardogValueFactory;
import com.complexible.stardog.api.Connection;
import com.complexible.stardog.api.ConnectionConfiguration;
import com.complexible.stardog.api.Getter;
import com.jbalint.bs.ontology.BsLib;
import com.jbalint.bs.ontology.Nepomuk;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.openrdf.model.IRI;
import org.openrdf.model.Model;

/**
 * Generate metadata structures for Bookstore db.
 */
public class FileMetadataGenerator {
	static final StardogValueFactory VF = StardogValueFactory.instance();

	Connection conn;
	Path root;
	Getter get;
	IRI context;

	/**
	 * Turn a Path into a model if it doesn't already exist in the DB and it's a regular file
	 */
	Model file(Path p) {
		try {
			if (Files.isRegularFile(p)) {
				get.reset();
				if (get.context(context).predicate(Nepomuk.NIE.url).object(getIRI(p)).statements().findFirst().isPresent()) {
					// already exists
					return null;
				}
				Files.getLastModifiedTime(p);
				ResourceBuilder mainFile = new ModelBuilder()
					                           .instance(Nepomuk.NFO.RemoteDataObject)
					                           .addProperty(Nepomuk.NIE.url, getIRI(p))
					                           .addProperty(BsLib.relativePath, p.relativize(root).toString())
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
		}
		catch (IOException theE) {
			theE.printStackTrace();
		}
		return null;
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
		FileMetadataGenerator fmg = new FileMetadataGenerator();
		fmg.conn = ConnectionConfiguration.at(args[0]);
		fmg.get = fmg.conn.get();
		fmg.root = Paths.get(args[1]);
		fmg.context = VF.createIRI(args[2]);
		MutableInt booksAdded = new MutableInt(0);
		Files.walk(fmg.root).map(fmg::file).filter(Objects::nonNull).forEach(m -> {
			fmg.conn.begin();
			fmg.conn.add().graph(m, fmg.context);
			fmg.conn.commit();
			booksAdded.increment();
		});
		System.err.println("Books added: " + booksAdded);
		//StatementSources.write(MemoryStatementSource.of(fmg.statements), RDFFormat.TURTLE, new FileOutputStream("/tmp/books.ttl"));
	}
}
