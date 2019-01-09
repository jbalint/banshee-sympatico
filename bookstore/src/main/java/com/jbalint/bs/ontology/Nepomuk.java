package com.jbalint.bs.ontology;

import com.stardog.stark.IRI;
import com.stardog.stark.Values;

/**
 * Nepomuk vocabularies
 *
 * http://www.semanticdesktop.org/ontologies/
 */
public class Nepomuk {

	/**
	 * Nepomuk Information Element. c.f. http://www.semanticdesktop.org/ontologies/2007/01/19/nie/
	 */
	public static class NIE {

		private static final String NS = "http://www.semanticdesktop.org/ontologies/2007/01/19/nie#";

		public static final IRI DataObject = Values.iri(NS, "DataObject"); // A unit of data that is created, annotated and processed on the user desktop. It represents a native structure the user works with. The usage of the term 'native' is important. It means that a DataObject can be directly mapped to a data structure maintained by a native application. This may be a file, a set of files or a part of a file. The granularity depends on the user. This class is not intended to be instantiated by itself. Use more specific subclasses."

		public static final IRI DataSource = Values.iri(NS, "DataSource"); // A superclass for all entities from which DataObjects can be extracted. Each entity represents a native application or some other system that manages information that may be of interest to the user of the Semantic Desktop. Subclasses may include FileSystems, Mailboxes, Calendars, websites etc. The exact choice of subclasses and their properties is considered application-specific. Each data extraction application is supposed to provide it's own DataSource ontology. Such an ontology should contain supported data source types coupled with properties necessary for the application to gain access to the data sources.  (paths, urls, passwords  etc...)"

		public static final IRI InformationElement = Values.iri(NS, "InformationElement"); // A unit of content the user works with. This is a superclass for all interpretations of a DataObject."

		public static final IRI Mode = Values.iri(NS, "Mode"); // Representation for a standard set of device/application/service modes, corresponding to various sets of modes that are either inbuilt in a device (e.g. inbuilt phone modes such as silent, loud, general, vibrate, etc.) or available for applications and online services (e.g. IM system modes such as busy, available, invisible, etc.)"

		public static final IRI byteSize = Values.iri(NS, "byteSize"); // The overall size of the data object in bytes. That means the space taken by the DataObject in its container, and not the size of the content that is of interest to the user. For cases where the content size is different (e.g. in compressed files the content is larger, in messages the content excludes headings and is smaller) use more specific properties, not necessarily subproperties of this one."

		public static final IRI characterSet = Values.iri(NS, "characterSet"); // Characterset in which the content of the InformationElement was created. Example: ISO-8859-1, UTF-8. One of the registered character sets at http://www.iana.org/assignments/character-sets. This characterSet is used to interpret any textual parts of the content. If more than one characterSet is used within one data object, use more specific properties."

		public static final IRI comment = Values.iri(NS, "comment"); // A user comment about an InformationElement."

		public static final IRI contentCreated = Values.iri(NS, "contentCreated"); // The date of the content creation. This may not necessarily be equal to the date when the DataObject (i.e. the physical representation) itself was created. Compare with nie:created property."

		public static final IRI contentModified = Values.iri(NS, "contentModified"); // The date of a modification of the original content (not its corresponding DataObject or local copy). Compare with nie:modified."

		public static final IRI informationElementDate = Values.iri(NS, "informationElementDate"); // A point or period of time associated with an event in the lifecycle of an Information Element. A common superproperty for all date-related properties of InformationElements in the NIE Framework."

		public static final IRI contentLastModified = Values.iri(NS, "contentLastModified"); // The date of the last modification of the original content (not its corresponding DataObject or local copy). Compare with nie:lastModified."

		public static final IRI contentSize = Values.iri(NS, "contentSize"); // The size of the content. This property can be used whenever the size of the content of an InformationElement differs from the size of the DataObject. (e.g. because of compression, encoding, encryption or any other representation issues). The contentSize in expressed in bytes."

		public static final IRI copyright = Values.iri(NS, "copyright"); // Content copyright"

		public static final IRI legal = Values.iri(NS, "legal"); // A common superproperty for all properties that point at legal information about an Information Element"

		public static final IRI coreGraph = Values.iri(NS, "coreGraph"); // Connects the data object with the graph that contains information about it. Deprecated in favor of a more generic nao:isDataGraphFor."

		public static final IRI created = Values.iri(NS, "created"); // Date of creation of the DataObject. Note that this date refers to the creation of the DataObject itself (i.e. the physical representation). Compare with nie:contentCreated."

		public static final IRI modified = Values.iri(NS, "modified"); // Date the DataObject was changed in any way.  Note that this date refers to the modification of the DataObject itself (i.e. the physical representation). Compare with nie:contentModified."

		public static final IRI dataSource = Values.iri(NS, "dataSource"); // Marks the provenance of a DataObject, what source does a data object come from."

		public static final IRI depends = Values.iri(NS, "depends"); // Dependency relation. A piece of content depends on another piece of data in order to be properly understood/used/interpreted."

		public static final IRI relatedTo = Values.iri(NS, "relatedTo"); // A common superproperty for all relations between a piece of content and other pieces of data (which may be interpreted as other pieces of content)."

		public static final IRI description = Values.iri(NS, "description"); // A textual description of the resource. This property may be used for any metadata fields that provide some meta-information or comment about a resource in the form of a passage of text. This property is not to be confused with nie:plainTextContent. Use more specific subproperties wherever possible."

		public static final IRI disclaimer = Values.iri(NS, "disclaimer"); // A disclaimer"

		public static final IRI generator = Values.iri(NS, "generator"); // Software used to "generate" the contents. E.g. a word processor name."

		public static final IRI generatorOption = Values.iri(NS, "generatorOption"); // A common superproperty for all settings used by the generating software. This may include compression settings, algorithms, autosave, interlaced/non-interlaced etc. Note that this property has no range specified and therefore should not be used directly. Always use more specific properties."

		public static final IRI hasLogicalPart = Values.iri(NS, "hasLogicalPart"); // Generic property used to express 'logical' containment relationships between InformationElements. NIE extensions are encouraged to provide more specific subproperties of this one. It is advisable for actual instances of InformationElement to use those specific subproperties. Note the difference between 'physical' containment (hasPart) and logical containment (hasLogicalPart)"

		public static final IRI isLogicalPartOf = Values.iri(NS, "isLogicalPartOf"); // Generic property used to express 'logical' containment relationships between DataObjects. NIE extensions are encouraged to provide more specific subproperties of this one. It is advisable for actual instances of InformationElement to use those specific subproperties. Note the difference between 'physical' containment (isPartOf) and logical containment (isLogicalPartOf)"

		public static final IRI hasPart = Values.iri(NS, "hasPart"); // Generic property used to express 'physical' containment relationships between DataObjects. NIE extensions are encouraged to provide more specific subproperties of this one. It is advisable for actual instances of DataObjects to use those specific subproperties. Note to the developers: Please be aware of the distinction between containment relation and provenance. The hasPart relation models physical containment, an InformationElement (a nmo:Message) can have a 'physical' part (an nfo:Attachment).  Also, please note the difference between physical containment (hasPart) and logical containment (hasLogicalPart) the former has more strict meaning. They may occur independently of each other."

		public static final IRI isPartOf = Values.iri(NS, "isPartOf"); // Generic property used to express containment relationships between DataObjects. NIE extensions are encouraged to provide more specific subproperties of this one. It is advisable for actual instances of DataObjects to use those specific subproperties. Note to the developers: Please be aware of the distinction between containment relation and provenance. The isPartOf relation models physical containment, a nie:DataObject (e.g. an nfo:Attachment) is a 'physical' part of an nie:InformationElement (a nmo:Message). Also, please note the difference between physical containment (isPartOf) and logical containment (isLogicalPartOf) the former has more strict meaning. They may occur independently of each other."

		public static final IRI htmlContent = Values.iri(NS, "htmlContent"); // The HTML content of an information element. This property can be used to store text including formatting in a generic fashion."

		public static final IRI identifier = Values.iri(NS, "identifier"); // An unambiguous reference to the InformationElement within a given context. Recommended best practice is to identify the resource by means of a string conforming to a formal identification system."

		public static final IRI interpretedAs = Values.iri(NS, "interpretedAs"); // Links the DataObject with the InformationElement it is interpreted as."

		public static final IRI isStoredAs = Values.iri(NS, "isStoredAs"); // Links the information element with the DataObject it is stored in."

		public static final IRI keyword = Values.iri(NS, "keyword"); // Adapted DublinCore: The topic of the content of the resource, as keyword. No sentences here. Recommended best practice is to select a value from a controlled vocabulary or formal classification scheme. "

		public static final IRI language = Values.iri(NS, "language"); // Language the InformationElement is expressed in. This property applies to the data object in its entirety. If the data object is divisible into parts expressed in multiple languages - more specific properties should be used. Users are encouraged to use the two-letter code specified in the RFC 3066"

		public static final IRI lastModified = Values.iri(NS, "lastModified"); // Last modification date of the DataObject. Note that this date refers to the modification of the DataObject itself (i.e. the physical representation). Compare with nie:contentLastModified."

		public static final IRI lastRefreshed = Values.iri(NS, "lastRefreshed"); // Date when information about this data object was retrieved (for the first time) or last refreshed from the data source. This property is important for metadata extraction applications that don't receive any notifications of changes in the data source and have to poll it regularly. This may lead to information becoming out of date. In these cases this property may be used to determine the age of data, which is an important element of it's dependability. "

		public static final IRI license = Values.iri(NS, "license"); // Terms and intellectual property rights licensing conditions."

		public static final IRI licenseType = Values.iri(NS, "licenseType"); // The type of the license. Possible values for this field may include "GPL", "BSD", "Creative Commons" etc."

		public static final IRI links = Values.iri(NS, "links"); // A linking relation. A piece of content links/mentions a piece of data"

		public static final IRI mimeType = Values.iri(NS, "mimeType"); // The mime type of the resource, if available. Example: "text/plain". See http://www.iana.org/assignments/media-types/. This property applies to data objects that can be described with one mime type. In cases where the object as a whole has one mime type, while it's parts have other mime types, or there is no mime type that can be applied to the object as a whole, but some parts of the content have mime types - use more specific properties."

		public static final IRI plainTextContent = Values.iri(NS, "plainTextContent"); // Plain-text representation of the content of a InformationElement with all markup removed. The main purpose of this property is full-text indexing and search. Its exact content is considered application-specific. The user can make no assumptions about what is and what is not contained within. Applications should use more specific properties wherever possible."

		public static final IRI rootElementOf = Values.iri(NS, "rootElementOf"); // DataObjects extracted from a single data source are organized into a containment tree. This property links the root of that tree with the datasource it has been extracted from"

		public static final IRI sourceMode = Values.iri(NS, "sourceMode"); // Represents a number of applicable modes for a data source."

		public static final IRI subject = Values.iri(NS, "subject"); // An overall topic of the content of a InformationElement"

		public static final IRI title = Values.iri(NS, "title"); // Name given to an InformationElement"

		public static final IRI url = Values.iri(NS, "url"); // URL of a DataObject. It points to the location of the object. A typial usage is FileDataObject. In cases where creating a simple file:// or http:// URL for a file is difficult (e.g. for files inside compressed archives) the applications are encouraged to use conventions defined by Apache Commons VFS Project at http://jakarta.apache.org/  commons/ vfs/ filesystems.html."

		public static final IRI version = Values.iri(NS, "version"); // The current version of the given data object. Exact semantics is unspecified at this level. Use more specific subproperties if needed."
	}

	/**
	 * Nepomuk File Ontology. c.f. http://www.semanticdesktop.org/ontologies/2007/03/22/nfo/
	 */
	public static class NFO {

		private static final String NS = "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#";

		public static final IRI horizontalResolution = Values.iri(NS, "horizontalResolution"); // Horizontal resolution of an image (if printed). Expressed in DPI."

		public static final IRI Image = Values.iri(NS, "Image"); // A file containing an image."

		public static final IRI sampleRate = Values.iri(NS, "sampleRate"); // The amount of audio samples per second."

		public static final IRI Audio = Values.iri(NS, "Audio"); // A file containing audio content"

		public static final IRI rate = Values.iri(NS, "rate"); // A common superproperty for all properties specifying the media rate. Examples of subproperties may include frameRate for video and sampleRate for audio. This property is expressed in units per second."

		public static final IRI HardDiskPartition = Values.iri(NS, "HardDiskPartition"); // A partition on a hard disk"

		public static final IRI fileName = Values.iri(NS, "fileName"); // Name of the file, together with the extension"

		public static final IRI FileDataObject = Values.iri(NS, "FileDataObject"); // A resource containing a finite sequence of bytes with arbitrary information, that is available to a computer program and is usually based on some kind of durable storage. A file is durable in the sense that it remains available for programs to use after the current program has finished."

		public static final IRI MediaStream = Values.iri(NS, "MediaStream"); // A stream of multimedia content, usually contained within a media container such as a movie (containing both audio and video) or a DVD (possibly containing many streams of audio and video). Most common interpretations for such a DataObject include Audio and Video."

		public static final IRI Presentation = Values.iri(NS, "Presentation"); // A Presentation made by some presentation software (Corel Presentations, OpenOffice Impress, MS Powerpoint etc.)"

		public static final IRI Document = Values.iri(NS, "Document"); // A generic document. A common superclass for all documents on the desktop."

		public static final IRI Media = Values.iri(NS, "Media"); // A piece of media content. This class may be used to express complex media containers with many streams of various media content (both aural and visual)."

		public static final IRI hashAlgorithm = Values.iri(NS, "hashAlgorithm"); // Name of the algorithm used to compute the hash value. Examples might include CRC32, MD5, SHA, TTH etc."

		public static final IRI FileHash = Values.iri(NS, "FileHash"); // A fingerprint of the file, generated by some hashing function."

		public static final IRI commentCharacterCount = Values.iri(NS, "commentCharacterCount"); // The amount of character in comments i.e. characters ignored by the compiler/interpreter."

		public static final IRI SourceCode = Values.iri(NS, "SourceCode"); // Code in a compilable or interpreted programming language."

		public static final IRI PlainTextDocument = Values.iri(NS, "PlainTextDocument"); // A file containing plain text (ASCII, Unicode or other encodings). Examples may include TXT, HTML, XML, program source code etc."

		public static final IRI TextDocument = Values.iri(NS, "TextDocument"); // A text document"

		public static final IRI foundry = Values.iri(NS, "foundry"); // The foundry, the organization that created the font."

		public static final IRI Font = Values.iri(NS, "Font"); // A font."

		public static final IRI CompressionType = Values.iri(NS, "CompressionType"); // Type of compression. Instances of this class represent the limited set of values allowed for the nfo:compressionType property."

		public static final IRI sideChannels = Values.iri(NS, "sideChannels"); // Number of side channels"

		public static final IRI channels = Values.iri(NS, "channels"); // Number of channels. This property is to be used directly if no detailed information is necessary. Otherwise use more detailed subproperties."

		public static final IRI interlaceMode = Values.iri(NS, "interlaceMode"); // True if the image is interlaced, false if not."

		public static final IRI Visual = Values.iri(NS, "Visual"); // File containing visual content."

		public static final IRI width = Values.iri(NS, "width"); // Visual content width in pixels."

		public static final IRI frameCount = Values.iri(NS, "frameCount"); // The amount of frames in a video sequence."

		public static final IRI Video = Values.iri(NS, "Video"); // A video file."

		public static final IRI count = Values.iri(NS, "count"); // A common superproperty for all properties signifying the amount of atomic media data units. Examples of subproperties may include sampleCount and frameCount."

		public static final IRI MediaFileListEntry = Values.iri(NS, "MediaFileListEntry"); // A single node in the list of media files contained within an MediaList instance. This class is intended to provide a type all those links have. In valid NRL untyped resources cannot be linked. There are no properties defined for this class but the application may expect rdf:first and rdf:last links. The former points to the DataObject instance, interpreted as Media the latter points at another MediaFileListEntr. At the end of the list there is a link to rdf:nil."

		public static final IRI Filesystem = Values.iri(NS, "Filesystem"); // A filesystem. Examples of filesystems include hard disk partitions, removable media, but also images thereof stored in files such as ISO."

		public static final IRI DataContainer = Values.iri(NS, "DataContainer"); // A superclass for all entities, whose primary purpose is to serve as containers for other data object. They usually don't have any "meaning" by themselves. Examples include folders, archives and optical disc images."

		public static final IRI filesystemType = Values.iri(NS, "filesystemType"); // Type of filesystem such as ext3 and ntfs."

		public static final IRI totalSpace = Values.iri(NS, "totalSpace"); // Total storage space of the filesystem, which can be different from nie:contentSize because the latter includes filesystem format overhead."

		public static final IRI freeSpace = Values.iri(NS, "freeSpace"); // Unoccupied storage space of the filesystem."

		public static final IRI occupiedSpace = Values.iri(NS, "occupiedSpace"); // Occupied storage space of the filesystem."

		public static final IRI uuid = Values.iri(NS, "uuid"); // Universally unique identifier of the filesystem. In the future, this property may have its parent changed to a more generic class."

		public static final IRI definesFunction = Values.iri(NS, "definesFunction"); // A name of a function/method defined in the given source code file."

		public static final IRI Archive = Values.iri(NS, "Archive"); // A compressed file. May contain other files or folder inside. "

		public static final IRI permissions = Values.iri(NS, "permissions"); // A string containing the permissions of a file. A feature common in many UNIX-like operating systems."

		public static final IRI lineCount = Values.iri(NS, "lineCount"); // The amount of lines in a text document"

		public static final IRI SoftwareItem = Values.iri(NS, "SoftwareItem"); // A DataObject representing a piece of software. Examples of interpretations of a SoftwareItem include an Application and an OperatingSystem."

		public static final IRI wordCount = Values.iri(NS, "wordCount"); // The amount of words in a text document."

		public static final IRI bookmarks = Values.iri(NS, "bookmarks"); // The address of the linked object. Usually a web URI."

		public static final IRI Bookmark = Values.iri(NS, "Bookmark"); // A bookmark of a webbrowser. Use nie:title for the name/label, nie:contentCreated to represent the date when the user added the bookmark, and nie:contentLastModified for modifications. nfo:bookmarks to store the link."

		public static final IRI characterPosition = Values.iri(NS, "characterPosition"); // Character position of the bookmark."

		public static final IRI pageNumber = Values.iri(NS, "pageNumber"); // Page linked by the bookmark."

		public static final IRI streamPosition = Values.iri(NS, "streamPosition"); // Stream position of the bookmark, suitable for e.g. audio books. Expressed in milliseconds"

		public static final IRI RemotePortAddress = Values.iri(NS, "RemotePortAddress"); // An address specifying a remote host and port. Such an address can be interpreted in many ways (examples of such interpretations include mailboxes, websites, remote calendars or filesystems), depending on an interpretation, various kinds of data may be extracted from such an address."

		public static final IRI Attachment = Values.iri(NS, "Attachment"); // A file attached to another data object. Many data formats allow for attachments: emails, vcards, ical events, id3 and exif..."

		public static final IRI EmbeddedFileDataObject = Values.iri(NS, "EmbeddedFileDataObject"); // A file embedded in another data object. There are many ways in which a file may be embedded in another one. Use this class directly only in cases if none of the subclasses gives a better description of your case."

		public static final IRI characterCount = Values.iri(NS, "characterCount"); // The amount of characters in the document."

		public static final IRI fileLastAccessed = Values.iri(NS, "fileLastAccessed"); // Time when the file was last accessed."

		public static final IRI supercedes = Values.iri(NS, "supercedes"); // States that a piece of software supercedes another piece of software."

		public static final IRI Software = Values.iri(NS, "Software"); // A piece of software. Examples may include applications and the operating system. This interpretation most commonly applies to SoftwareItems."

		public static final IRI programmingLanguage = Values.iri(NS, "programmingLanguage"); // Indicates the name of the programming language this source code file is written in. Examples might include 'C', 'C++', 'Java' etc."

		public static final IRI PaginatedTextDocument = Values.iri(NS, "PaginatedTextDocument"); // A file containing a text document, that is unambiguously divided into pages. Examples might include PDF, DOC, PS, DVI etc."

		public static final IRI Application = Values.iri(NS, "Application"); // An application"

		public static final IRI sampleCount = Values.iri(NS, "sampleCount"); // The amount of samples in an audio clip."

		public static final IRI height = Values.iri(NS, "height"); // Visual content height in pixels."

		public static final IRI frontChannels = Values.iri(NS, "frontChannels"); // Number of front channels."

		public static final IRI FilesystemImage = Values.iri(NS, "FilesystemImage"); // An image of a filesystem. Instances of this class may include CD images, DVD images or hard disk partition images created by various pieces of software (e.g. Norton Ghost). Deprecated in favor of nfo:Filesystem."

		public static final IRI ArchiveItem = Values.iri(NS, "ArchiveItem"); // A file entity inside an archive."

		public static final IRI rearChannels = Values.iri(NS, "rearChannels"); // Number of rear channels."

		public static final IRI bitsPerSample = Values.iri(NS, "bitsPerSample"); // Amount of bits in each audio sample."

		public static final IRI bitDepth = Values.iri(NS, "bitDepth"); // A common superproperty for all properties signifying the amount of bits for an atomic unit of data. Examples of subproperties may include bitsPerSample and bitsPerPixel"

		public static final IRI HtmlDocument = Values.iri(NS, "HtmlDocument"); // A HTML document, may contain links to other files."

		public static final IRI duration = Values.iri(NS, "duration"); // Duration of a media piece."

		public static final IRI lfeChannels = Values.iri(NS, "lfeChannels"); // Number of Low Frequency Expansion (subwoofer) channels."

		public static final IRI hasMediaStream = Values.iri(NS, "hasMediaStream"); // Connects a media container with a single media stream contained within."

		public static final IRI Spreadsheet = Values.iri(NS, "Spreadsheet"); // A spreadsheet, created by a spreadsheet application. Examples might include Gnumeric, OpenOffice Calc or MS Excel."

		public static final IRI isPasswordProtected = Values.iri(NS, "isPasswordProtected"); // States if a given resource is password-protected."

		public static final IRI hashValue = Values.iri(NS, "hashValue"); // The actual value of the hash."

		public static final IRI EncryptionStatus = Values.iri(NS, "EncryptionStatus"); // The status of the encryption of an InformationElement. nfo:encryptedStatus means that the InformationElement has been encrypted and couldn't be decrypted by the extraction software, thus no content is available. nfo:decryptedStatus means that decryption was successfull and the content is available."

		public static final IRI uncompressedSize = Values.iri(NS, "uncompressedSize"); // Uncompressed size of the content of a compressed file."

		public static final IRI deletionDate = Values.iri(NS, "deletionDate"); // The date and time of the deletion."

		public static final IRI DeletedResource = Values.iri(NS, "DeletedResource"); // A file entity that has been deleted from the original source. Usually such entities are stored within various kinds of 'Trash' or 'Recycle Bin' folders."

		public static final IRI MindMap = Values.iri(NS, "MindMap"); // A MindMap, created by a mind-mapping utility. Examples might include FreeMind or mind mapper."

		public static final IRI SoftwareService = Values.iri(NS, "SoftwareService"); // A service published by a piece of software, either by an operating system or an application. Examples of such services may include calendar, addressbook and mailbox managed by a PIM application. This category is introduced to distinguish between data available directly from the applications (Via some Interprocess Communication Mechanisms) and data available from files on a disk. In either case both DataObjects would receive a similar interpretation (e.g. a Mailbox) and wouldn't differ on the content level."

		public static final IRI originalLocation = Values.iri(NS, "originalLocation"); // The original location of the deleted resource."

		public static final IRI Website = Values.iri(NS, "Website"); // A website, usually a container for remote resources, that may be interpreted as HTMLDocuments, images or other types of content."

		public static final IRI Cursor = Values.iri(NS, "Cursor"); // A Cursor."

		public static final IRI RasterImage = Values.iri(NS, "RasterImage"); // A raster image."

		public static final IRI hasMediaFileListEntry = Values.iri(NS, "hasMediaFileListEntry"); // This property is intended to point to an RDF list of MediaFiles."

		public static final IRI MediaList = Values.iri(NS, "MediaList"); // A file containing a list of media files.e.g. a playlist"

		public static final IRI BookmarkFolder = Values.iri(NS, "BookmarkFolder"); // A folder with bookmarks of a webbrowser. Use nfo:containsBookmark to relate Bookmarks. Folders can contain subfolders, use containsBookmarkFolder to relate them."

		public static final IRI colorDepth = Values.iri(NS, "colorDepth"); // Amount of bits used to express the color of each pixel."

		public static final IRI averageBitrate = Values.iri(NS, "averageBitrate"); // The average overall bitrate of a media container. (i.e. the size of the piece of media in bits, divided by it's duration expressed in seconds)."

		public static final IRI Icon = Values.iri(NS, "Icon"); // An Icon (regardless of whether it's a raster or a vector icon. A resource representing an icon could have two types (Icon and Raster, or Icon and Vector) if required."

		public static final IRI fileOwner = Values.iri(NS, "fileOwner"); // The owner of the file as defined by the file system access rights feature."

		public static final IRI aspectRatio = Values.iri(NS, "aspectRatio"); // Visual content aspect ratio. (Width divided by Height)"

		public static final IRI containsBookmarkFolder = Values.iri(NS, "containsBookmarkFolder"); // The folder contains a bookmark folder."

		public static final IRI belongsToContainer = Values.iri(NS, "belongsToContainer"); // Models the containment relations between Files and Folders (or CompressedFiles)."

		public static final IRI verticalResolution = Values.iri(NS, "verticalResolution"); // Vertical resolution of an Image (if printed). Expressed in DPI"

		public static final IRI fileUrl = Values.iri(NS, "fileUrl"); // URL of the file. It points at the location of the file. In cases where creating a simple file:// or http:// URL for a file is difficult (e.g. for files inside compressed archives) the applications are encouraged to use conventions defined by Apache Commons VFS Project at http://jakarta.apache.org/  commons/ vfs/ filesystems.html."

		public static final IRI frameRate = Values.iri(NS, "frameRate"); // Amount of video frames per second."

		public static final IRI fontFamily = Values.iri(NS, "fontFamily"); // The name of the font family."

		public static final IRI fileCreated = Values.iri(NS, "fileCreated"); // File creation date"

		public static final IRI bitrateType = Values.iri(NS, "bitrateType"); // The type of the bitrate. Examples may include CBR and VBR."

		public static final IRI encoding = Values.iri(NS, "encoding"); // The encoding used for the Embedded File. Examples might include BASE64 or UUEncode"

		public static final IRI Folder = Values.iri(NS, "Folder"); // A folder/directory. Examples of folders include folders on a filesystem and message folders in a mailbox."

		public static final IRI hasHash = Values.iri(NS, "hasHash"); // Links the file with it's hash value."

		public static final IRI codec = Values.iri(NS, "codec"); // The name of the codec necessary to decode a piece of media."

		public static final IRI fileLastModified = Values.iri(NS, "fileLastModified"); // last modification date"

		public static final IRI compressionType = Values.iri(NS, "compressionType"); // The type of the compression. Values include, 'lossy' and 'lossless'."

		public static final IRI pageCount = Values.iri(NS, "pageCount"); // Number of pages."

		public static final IRI definesGlobalVariable = Values.iri(NS, "definesGlobalVariable"); // Name of a global variable defined within the source code file."

		public static final IRI Trash = Values.iri(NS, "Trash"); // Represents a container for deleted files, a feature common in modern operating systems."

		public static final IRI conflicts = Values.iri(NS, "conflicts"); // States that a piece of software is in conflict with another piece of software."

		public static final IRI encryptionStatus = Values.iri(NS, "encryptionStatus"); // The status of the encryption of the InformationElement."

		public static final IRI containsBookmark = Values.iri(NS, "containsBookmark"); // The folder contains a bookmark."

		public static final IRI Executable = Values.iri(NS, "Executable"); // An executable file."

		public static final IRI definesClass = Values.iri(NS, "definesClass"); // Name of a class defined in the source code file."

		public static final IRI OperatingSystem = Values.iri(NS, "OperatingSystem"); // An OperatingSystem"

		public static final IRI fileSize = Values.iri(NS, "fileSize"); // The size of the file in bytes. For compressed files it means the size of the packed file, not of the contents. For folders it means the aggregated size of all contained files and folders "

		public static final IRI RemoteDataObject = Values.iri(NS, "RemoteDataObject"); // A file data object stored at a remote location. Don't confuse this class with a RemotePortAddress. This one applies to a particular resource, RemotePortAddress applies to an address, that can have various interpretations."

		public static final IRI colorCount = Values.iri(NS, "colorCount"); // The number of colors used/available in a raster image."

		public static final IRI paletteSize = Values.iri(NS, "paletteSize"); // The number of colors defined in palette of the raster image."

		public static final IRI WebDataObject = Values.iri(NS, "WebDataObject"); // An information resources of which representations (files, streams) can be retrieved through a web server. They may be generated at retrieval time. Typical examples are pages served by PHP or AJAX or mp3 streams."

		public static final IRI LocalFileDataObject = Values.iri(NS, "LocalFileDataObject"); // A local file data object which is stored on a local file system. Its nie:url always uses the file:/ protocol. The main use of this class is to distinguish local and non-local files."

		public static final IRI depiction = Values.iri(NS, "depiction"); // Relates an information element to an image which depicts said element."

		public static final IRI depicts = Values.iri(NS, "depicts"); // Relates an image to the information elements it depicts."

		public static final IRI PlacemarkContainer = Values.iri(NS, "PlacemarkContainer"); // A data object containing placemark(s). Use nie:contentCreated to represent the date when the user created the dataobject, nao:creator for defining the creator, nie:contentLastModified for modifications. nfo:containsPlacemark to refer to individual placemarks within."

		public static final IRI containsPlacemark = Values.iri(NS, "containsPlacemark"); // Containment relation between placemark containers (files) and placemarks within."

		public static final IRI Placemark = Values.iri(NS, "Placemark"); // One placemark within a placemark container/file. Use nie:title for the name/label, nao:creator for defining the creator."
	}

	/**
	 * Digital.Me Context Ontology. c.f. http://www.semanticdesktop.org/ontologies/2011/10/05/dcon/
	 */
	public static class DCON {

		private static final String NS = "http://www.semanticdesktop.org/ontologies/2011/10/05/dcon#";

		public static final IRI Context = Values.iri(NS, "Context"); // This RDF Graph contains information that can be used to characterise the state of an entity, where an entity can be a user's device or the user themselves. Although this abstract class is not intended for direct use, there are two main distinguishible subclasses. dcon:LiveContext represents uninterpreted states for a user's (multiple) devices, whereas dcon:Situation represents interpreted user states. There are three levels of representation for context: Aspects at the highest level, a set of Elements for each aspect, and a number of Attributes for each element."

		public static final IRI Aspect = Values.iri(NS, "Aspect"); // At the highlest level, context information is classified under a number of context Aspects. These are provided as subclasses of this abstract class, which is not intended for direct use."

		public static final IRI Element = Values.iri(NS, "Element"); // Each context Aspect has a number of specific elements. These are linked to each aspect through the generic dcon:hasContextElement property. As this is an abstract class, it is not intended for direct use. Instead, element instances are typically inferred through the use of dcon:hasContextElement (and its subproperties) and dcon:hasObservation. Elements can have multiple observations (e.g. from a different source or device), linked through dcon:hasObservation."

		public static final IRI Observation = Values.iri(NS, "Observation"); // Each context Element can have one or more observations, e.g. from different source or device. These are linked to each element through the dcon:hasObservation property. In order to inherit the context attributes relevant for the element's type, observations need to be multi-typed. For example, each observation of a nie:InformationElement must also be defined as a nie:InformationElement instance, so that it can inherit all context attributes having the element's type in their domain specification. However, this does not mean that an observation is also an element, but merely that it must have the same type. Other defining properties of an observation are the data source (dcon:recordedBy) and the observation time (dcon:recordedAt). A validity property (dcon:validity) is also provided, and is intended to regulate the time frame during which an observation is considered valid."

		public static final IRI LiveContext = Values.iri(NS, "LiveContext"); // This class represents the uninterpreted state of the users as sensed through their devices. Instances of this class should be limited, since they are meant to represent realtime, 'live' raw context information as sensed through a variety of a user's devices, their applications and sensors. Although one unique instance should represent the dynamically changing live context, additional instances can be used to contain live context information for the recent past."

		public static final IRI Situation = Values.iri(NS, "Situation"); // This class represents one or more interpreted user states. A user situation refers to a generalised context graph that is derived from the merging of a number of positive and negative previous live context instances. These instances are linked though dcon:positiveInstance and dcon:negativeInstance. Unlike live context, situations are independent of time and have the possibility to recur. As opposed to live context, situations are also independent of devices and are instead user-centric."

		public static final IRI Schedule = Values.iri(NS, "Schedule"); // The schedule aspect covers context elements relating to personal scheduling. Typically this includes elements managed by calendaring or task management tools."

		public static final IRI Peers = Values.iri(NS, "Peers"); // The peers aspect covers context elements relating to individuals or groups surrounding the user."

		public static final IRI Environment = Values.iri(NS, "Environment"); // The environment aspect covers context elements relating to current or forecasted environmental conditions, as experienced directly by the user, or by sensors in their surroundings."

		public static final IRI Attention = Values.iri(NS, "Attention"); // The attention aspect covers context elements relating to the users' activities on their digital devices."

		public static final IRI SpaTem = Values.iri(NS, "SpaTem"); // The spatio-temporal aspect covers context elements relating to the current user's physical locations, movement and time."

		public static final IRI Connectivity = Values.iri(NS, "Connectivity"); // The connectivity aspect covers context elements relating to the digital networks that are in range of the users' devices."

		public static final IRI State = Values.iri(NS, "State"); // The state aspect covers context elements relating to the state of either the user or their devices."

		public static final IRI hasLiveContext = Values.iri(NS, "hasLiveContext"); // Links the special live context representation instance to the user. Past live contexts are logged through the use of the DUHO ontology."

		public static final IRI hasSituation = Values.iri(NS, "hasSituation"); // Links one or more active/current situations to the user. Situations are stored permanently, whether currently active or otherwise."

		public static final IRI positiveInstance = Values.iri(NS, "positiveInstance"); // Situations are characterised through a series of past live context instances, required for its training. This property points to one or more positive instances, i.e., past actual live contexts snapshots during which it was determined that the abstract situation was active."

		public static final IRI negativeInstance = Values.iri(NS, "negativeInstance"); // Situations are characterised through a series of past live context instances, required for its training. This property points to one or more negative instances, i.e., past actual live contexts snapshots during which it was determined that the abstract situation was not active."

		public static final IRI hasObservation = Values.iri(NS, "hasObservation"); // As context Elements can have one or more observations, this property links each of them to the respective element."

		public static final IRI hasContextElement = Values.iri(NS, "hasContextElement"); // Context aspects categorise elements relating to the same category of context information. Individual context elements are linked to each aspect through this generic property, which is not intended for direct use. Instead, specific subproperties for each supported element is provided."

		public static final IRI hasContextAttribute = Values.iri(NS, "hasContextAttribute"); // Amongst other static attributes, context elements (or rather, their observations) have various context-related attributes, i.e. attributes which are bound to a particular time and context. This generic property links this type of attributes to the elements' observations, and is not intended for direct use. Instead, specific subproperties are provided for each supported element type. In order for observations to inherit these subproperties, each needs to be typed according to the element's type (see note in dcon:Observation comment)."

		public static final IRI isRequired = Values.iri(NS, "isRequired"); // This property marks those context elements which are necessary for a situation to occurr. If a required element is not active, then the correspoding situation is excluded from candidate active situations."

		public static final IRI isExcluder = Values.iri(NS, "isExcluder"); // This property marks those context elements whose presence excludes a situation from occurring. If such an element is active, then the correspoding situation is excluded from candidate active situations."

		public static final IRI validity = Values.iri(NS, "validity"); // Live context element observations have different validity periods, which mean they remain active only for a specified time interval after they have been sensed or otherwise detected. The value for this property denotes the time when the element observation's relevance expires."

		public static final IRI weight = Values.iri(NS, "weight"); // This property assigns different weights to specific context aspects and/or elements, in order to better characterise a situation. As weights can be attached to both dcon:Element and dcon:Aspect instances, the domain of this property is generalised to dcon:Observation. However, the attachment of weights to other instances is not considered correct application of the DCON vocabulary. Expected weight values range from 0 (no impact on situation), to 1 (high impact on a situation). In addition, the dcon:isRequired property defines a necessity relationship between a context element and a situation, whereas the dcon:isExcluder property defines the inverse."

		public static final IRI recordedBy = Values.iri(NS, "recordedBy"); // This property ties each element's observation to the data source that recorded it, specifically data sources of type account (dao:Account subclass) or device (ddo:Device subclass)."

		public static final IRI recordedAt = Values.iri(NS, "recordedAt"); // This property stores the time at which each element's observation has been generated, or last updated. Whereas for situations only the time of first occurrence is recorded, live context instances always carry the actual time."

		public static final IRI currentMode = Values.iri(NS, "currentMode"); // Refers to the mode of the data source from which a context element has been retrieved, at retrieval time. A data source can consist of a service, an account, an application, or a device.  "

		public static final IRI lastActive = Values.iri(NS, "lastActive"); // The last date and time for which user activity was detected on a device. This could consist of the last movement detected from the cursor/keyboard/touch screen and other device input mechanisms, as well as the last call recorded for voice communication-enabled devices."

		public static final IRI lastMovementRegistered = Values.iri(NS, "lastMovementRegistered"); // The last date and time for which a portable device was detected to be moving. This information relies on embedded accelerometers and other motion sensors."

		public static final IRI currentEvent = Values.iri(NS, "currentEvent"); // Refers to an ongoing event characterising a context instance."

		public static final IRI upcomingEvent = Values.iri(NS, "upcomingEvent"); // Refers to an upcoming event characterising a context instance."

		public static final IRI upcomingTask = Values.iri(NS, "upcomingTask"); // Refers to an upcoming task characterising a context instance."

		public static final IRI currentTask = Values.iri(NS, "currentTask"); // Refers to an ongoing task characterising a context instance."

		public static final IRI nearbyPerson = Values.iri(NS, "nearbyPerson"); // Refers to individuals characterising a context instance."

		public static final IRI nearbyGroup = Values.iri(NS, "nearbyGroup"); // Refers to a group of individuals characterising a context instance. Includes both user-defined groups, and ad-hoc groups recognised by the system."

		public static final IRI currentBrightness = Values.iri(NS, "currentBrightness"); // Refers to a pre-defined environmental brightness level characterising a context instance."

		public static final IRI currentNoise = Values.iri(NS, "currentNoise"); // Refers to a pre-defined environmental noise level characterising a context instance."

		public static final IRI currentWeather = Values.iri(NS, "currentWeather"); // Refers to a pre-defined weather condition characterising a context instance."

		public static final IRI forecastWeather = Values.iri(NS, "forecastWeather"); // Refers to a forecast, pre-defined weather condition characterising a context instance."

		public static final IRI currentTemperature = Values.iri(NS, "currentTemperature"); // Refers to a pre-defined environmental temperature range characterising a context instance."

		public static final IRI activeApplication = Values.iri(NS, "activeApplication"); // Refers to active applications (running on a device) characterising a context instance."

		public static final IRI activeFile = Values.iri(NS, "activeFile"); // Refers to active files (running on a device) characterising a context instance."

		public static final IRI nearbyEvent = Values.iri(NS, "nearbyEvent"); // Refers to nearby events that characterise a context instance."

		public static final IRI nearbyPlace = Values.iri(NS, "nearbyPlace"); // Refers to a nearby place type or specific location that characterises a context instance."

		public static final IRI currentPlace = Values.iri(NS, "currentPlace"); // Refers to a place type or specific location that characterises a context instance."

		public static final IRI currentTime = Values.iri(NS, "currentTime"); // Refers to a pre-defined time period that characterises a context instance."

		public static final IRI averageSpeed = Values.iri(NS, "averageSpeed"); // Refers to a pre-defined speed range characterising a context instance."

		public static final IRI averageDirection = Values.iri(NS, "averageDirection"); // Refers to a pre-defined traveling direction characterising a context instance."

		public static final IRI currentAltitude = Values.iri(NS, "currentAltitude"); // States the current altitude (metres above sea level)"

		public static final IRI currentAbsoluteAltitude = Values.iri(NS, "currentAbsoluteAltitude"); // States the current absolute altitude (metres above ground level)"

		public static final IRI connection = Values.iri(NS, "connection"); // Refers to a pre-defined traveling direction characterising a context instance."

		public static final IRI currentActivity = Values.iri(NS, "currentActivity"); // Refers to a pre-defined activity category characterising a context instance."

		public static final IRI currentAvailability = Values.iri(NS, "currentAvailability"); // Refers to a pre-defined personal availability characterising a context instance."

		public static final IRI uvindex = Values.iri(NS, "uvindex"); // Registers an actual UV index value element attribute to an active weather condition in the current context."

		public static final IRI humidity = Values.iri(NS, "humidity"); // Registers an actual humidity level value element attribute to an active weather condition in the current context."

		public static final IRI precipitation = Values.iri(NS, "precipitation"); // Registers an actual precipitation value element attribute to an active weather condition in the current context."

		public static final IRI cloudcover = Values.iri(NS, "cloudcover"); // Registers an actual cloud cover percentage value element attribute to an active weather condition in the current context."

		public static final IRI snowfalldegree = Values.iri(NS, "snowfalldegree"); // Registers an actual snowfall degree value element attribute to an active weather condition in the current context."

		public static final IRI stormdegree = Values.iri(NS, "stormdegree"); // Registers an actual storm degree value element attribute to an active weather condition in the current context."

		public static final IRI fogfactor = Values.iri(NS, "fogfactor"); // Registers an actual fog factor value element attribute to an active weather condition in the current context."

		public static final IRI windscale = Values.iri(NS, "windscale"); // Registers an actual wind scale value element attribute to an active weather condition in the current context."

		public static final IRI temperature = Values.iri(NS, "temperature"); // Registers an actual temperature value element attribute to an active weather condition or temperature range in the current context. As this context attribute applies to instances of both dpo:Temperature and dpo:WeatherConditions in the DPO Presence Ontology, the domain has been generalised to the first common DPO superclass. However, the attachment of this attribute to other instances is not considered correct application of the DCON vocabulary."

		public static final IRI brightnessLevel = Values.iri(NS, "brightnessLevel"); // Registers an actual environmental brightness value element attribute to an active brightness level range in the current context."

		public static final IRI noiseLevel = Values.iri(NS, "noiseLevel"); // Registers an actual environmental noise level value element attribute to an active noise level range in the current context."

		public static final IRI musicDetected = Values.iri(NS, "musicDetected"); // Registers whether music has been detected in the active environmental noise range in the current context."

		public static final IRI voicesDetected = Values.iri(NS, "voicesDetected"); // Registers how many voices have been detected in the active environmental noise range in the current context."

		public static final IRI inForeground = Values.iri(NS, "inForeground"); // Specifies whether an active file(running on a device) is in the foreground (true) or background (false) in the current context."

		public static final IRI writeable = Values.iri(NS, "writeable"); // Specifies whether an active file (running on a device) is in read-and-write (true) or read-only mode (false) in the current context."

		public static final IRI inEditMode = Values.iri(NS, "inEditMode"); // Specifies whether a writeable active file (running on a device) is being edited/written to, in the current context."

		public static final IRI dateTime = Values.iri(NS, "dateTime"); // States the exact date and time value for a pre-defined time period in the current context."

		public static final IRI minutes = Values.iri(NS, "minutes"); // Derived from the dateTime, this property stores the exact amount of minutes elapsed since the start of the current day. Implicitly also stores the number of elapsed hours. Thus, a value of 0 means that the current day has just started whereas a value of 1439 indicates the end of a day."

		public static final IRI dayMonth = Values.iri(NS, "dayMonth"); // Derived from the dateTime, this property stores the number of days elapsed for the current month, where 1 indicates the first day of the month, up to a maximum value of 31."

		public static final IRI dayWeek = Values.iri(NS, "dayWeek"); // Derived from the dateTime, this property stores the number of days elapsed for the current week, where 1 indicates a Monday, and 7 a Sunday."

		public static final IRI month = Values.iri(NS, "month"); // Derived from the dateTime, this property stores the number of months elapsed for the current year, where 1 indicates January, and 12 December."

		public static final IRI direction = Values.iri(NS, "direction"); // States the actual direction value for a pre-defined direction range in the current context."

		public static final IRI speed = Values.iri(NS, "speed"); // States the actual speed value for a pre-defined user movemement range in the current context."

		public static final IRI altitude = Values.iri(NS, "altitude"); // States the current altitude (metres above sea level)"

		public static final IRI absoluteAltitude = Values.iri(NS, "absoluteAltitude"); // States the current absolute altitude (metres above ground level)"

		public static final IRI distance = Values.iri(NS, "distance"); // Records the approximate distance between a device (as a proxy for the user) and an item. Not to be confused with dcon:distanceCovered. As this context attribute can be attached to instances of ncal:Event, pimo:Location, dpo:Place and pimo:Person, the domain of this property is generalised to dcon:Observation. However, the attachment of this attribute to other instances is not considered correct application of the DCON vocabulary."

		public static final IRI latitude = Values.iri(NS, "latitude"); // Records the exact latitude of the approximated location"

		public static final IRI longitude = Values.iri(NS, "longitude"); // Records the exact longitude of the approximated location"

		public static final IRI connected = Values.iri(NS, "connected"); // Specifies whether a network in range of the user's device(s) is connected (true) or otherwise (false) in the current context."

		public static final IRI signal = Values.iri(NS, "signal"); // States the actual signal strength value for a network connection in the current context."

		public static final IRI networkSpeed = Values.iri(NS, "networkSpeed"); // States the actual speed value for a network connection in the current context."

		public static final IRI caloriesExpended = Values.iri(NS, "caloriesExpended"); // Specifies the amount of calories burnt while performing the activity."

		public static final IRI distanceCovered = Values.iri(NS, "distanceCovered"); // Specifies the distance travelled while performing the activity."

		public static final IRI duration = Values.iri(NS, "duration"); // Specifies the duration since the start of the activity."
	}
}
