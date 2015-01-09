package org.raisercostin.util.io

import java.nio.file.Paths
import java.io.File
import java.nio.file.Path
import org.apache.commons.io.FileUtils
import java.io.FileOutputStream
import java.io.OutputStream
import org.apache.commons.io.FilenameUtils
import java.io.InputStream
import scala.io.BufferedSource
import java.io.FileInputStream
import java.io.ByteArrayInputStream
import java.io.OutputStreamWriter
import java.io.Writer
import scala.io.Source
import java.io.BufferedOutputStream
import java.io.BufferedWriter
import java.io.PrintWriter
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import Constants._
import java.util.regex.Pattern
import scala.util.Failure
import org.apache.commons.io.IOUtils
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.Files

private object Constants {
  val FILE_SEPARATOR = File.separator
}

trait NavigableLocation {
  val logger = org.slf4j.LoggerFactory.getLogger("locations")
  def child(child: String): this.type
  def parent: this.type
  def withParent(process: (this.type) => Any): this.type = {
    process(parent)
    this
  }
  def descendant(childs: Seq[String]): this.type = if (childs.isEmpty) this else child(childs.head).descendant(childs.tail)
}
trait BaseLocation extends NavigableLocation {
  def raw: String
  def extension: String = FilenameUtils.getExtension(absolute)
  def name: String = FilenameUtils.getName(absolute)
  def baseName: String = FilenameUtils.getBaseName(absolute)
  def parentName: String = toFile.getParentFile.getAbsolutePath
  /**To read data you should read the inputstream*/
  def toUrl: java.net.URL = toFile.toURI.toURL
  def toFile: File
  def toPath: Path = toFile.toPath
  def toInputStream: InputStream
  def toPath(subFile: String): Path = toPath.resolve(subFile)

  def toSource: scala.io.BufferedSource = {
    //import org.apache.commons.io.input.BOMInputStream
    //import org.apache.commons.io.IOUtils
    //def toBomInputStream: InputStream = new BOMInputStream(toInputStream,false)
    import java.nio.charset.Charset
    import java.nio.charset.CodingErrorAction
    val decoder = Charset.forName("UTF-8").newDecoder()
    decoder.onMalformedInput(CodingErrorAction.IGNORE)
    scala.io.Source.fromInputStream(toInputStream)(decoder)
    //def toSource: BufferedSource = scala.io.Source.fromInputStream(toInputStream, "UTF-8")
  }
  def absolute: String = toPath("").toAbsolutePath.toString
  def extractAncestor(ancestor: BaseLocation): Try[Seq[String]] = diff(absolute, ancestor.absolute).map { _.split(Pattern.quote(FILE_SEPARATOR)).filterNot(_.trim.isEmpty) }
  def relativeTo(ancestor: BaseLocation) = extractAncestor(ancestor).get.foldLeft("")((x, y) => (if (x.isEmpty) "" else (x + FILE_SEPARATOR)) + y)
  def diff(text: String, prefix: String) = if (text.startsWith(prefix)) Success(text.substring(prefix.length)) else Failure(new RuntimeException(s"Text [$text] doesn't start with [$prefix]."))
  def isAbsolute = toFile.isAbsolute()
  def mkdirIfNecessary: this.type = {
    FileUtils.forceMkdir(toFile)
    this
  }
  def parent: this.type
  def mkdirOnParentIfNecessary: this.type = {
    parent.mkdirIfNecessary
    this
  }
  def pathInRaw: String = {
    val a = raw.replaceAll("""^([^*]*)[*].*$""", "$1");
    println(a);
    a
  }
  def list: Iterator[InputLocation] = Option(existing).map(_.toFile.listFiles.toIterator).getOrElse(Iterator()).map(Locations.file(_))
  def traverse: Traversable[(Path, BasicFileAttributes)] = if (raw contains "*")
    Locations.file(pathInRaw).parent.traverse
  else
    new FileVisitor.TraversePath(toPath)
  def traverseFiles = if (exists) traverse.map { case (file, attr) => file } else Traversable()

  def traverseWithDir = new FileVisitor.TraversePath(toPath, true)
  protected def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  def hasDirs = RichPath.wrapPath(toPath).list.find(_.toFile.isDirectory).nonEmpty
  def isFile = toFile.isFile
  def exists = toFile.exists
  def existing: this.type =
    if (toFile.exists)
      this
    else
      throw new RuntimeException("[" + this + "] doesn't exist!")
  def existingOption: Option[this.type] =
    if (exists)
      Some(this)
    else
      None
  def existing(source: BufferedSource) = {
    //if (source.nonEmpty)
    val hasNext = Try { source.hasNext }
    //println(s"$absolute hasNext=$hasNext")
    val hasNext2 = hasNext.recover {
      case ex: Throwable =>
        throw new RuntimeException("[" + this + "] doesn't exist!")
    }
    //hasNext might be false if is empty
    //    if (!hasNext2.get)
    //      throw new RuntimeException("[" + this + "] doesn't have next!")
    source
  }
  def inspect(message: (this.type) => String): this.type = {
    logger.info(message(this))
    this
  }
  def withBaseName(baseNameSupplier: String => String): this.type = parent.child(withExtension2(baseNameSupplier(baseName), extension))
  def withName(nameSupplier: String => String): this.type = parent.child(nameSupplier(name))
  def withExtension(extensionSupplier: String => String): this.type = parent.child(withExtension2(baseName, extensionSupplier(extension)))
  protected def withExtension2(name: String, ext: String) =
    if (ext.length > 0)
      name + "." + ext
    else name
}
trait InputLocation extends BaseLocation {
  def toInputStream: InputStream = new FileInputStream(absolute)
  //def child(child: String): InputLocation
  //def parent: InputLocation.this.type
  def bytes: Array[Byte] = org.apache.commons.io.FileUtils.readFileToByteArray(toFile)
  def usingInputStream(op: InputStream => Unit): Unit =
    using(toInputStream)(inputStream => op(inputStream))
  def readLines =
    existing(toSource).getLines
  def copyTo(dest: OutputLocation) = {
    //overwrite
    FileUtils.copyInputStreamToFile(toInputStream, dest.toFile)
  }
  def readContent = {
    // Read a file into a string
    //    import rapture._
    //    import core._, io._, net._, uri._, json._, codec._
    //    import encodings.`UTF-8`
    //    val src = uri"http://rapture.io/sample.json".slurp[Char]
    //existing(toSource).getLines mkString ("\n")
    try { IOUtils.toString(toInputStream) } catch { case x: Throwable => throw new RuntimeException("While reading " + this, x) }
  }
  def readContentAsText: Try[String] =
    Try(readContent)
  //Try(existing(toSource).getLines mkString ("\n"))
  def unzip: ZipInputLocation = ???
}
trait OutputLocation extends BaseLocation {
  def asInput: InputLocation
  def append: Boolean
  def toOutputStream: OutputStream = new FileOutputStream(absolute, append)
  def toWriter: Writer = new BufferedWriter(new OutputStreamWriter(toOutputStream, "UTF-8"))
  def toPrintWriter: PrintWriter = new PrintWriter(new OutputStreamWriter(toOutputStream, StandardCharsets.UTF_8), true)
  def rename(renamer: String => String) = {
    val newName = renamer(baseName)
    if (newName == baseName) {
      //println(s"ignore [${absolute}] to [${absolute}]")
    } else {
      val dest = parent.child(withExtension2(newName, extension))
      println(s"move [${absolute}] to [${dest.absolute}]")
      FileUtils.moveFile(toFile, dest.toFile)
    }
  }
  def moveTo(dest: OutputLocation): this.type = {
    FileUtils.moveFile(toFile, dest.toFile)
    this
  }
  def deleteIfExists: this.type = {
    if (exists) {
      logger.info(s"delete existing $absolute")
      FileUtils.forceDelete(toFile)
    }
    this
  }
  def usingOutputStream(op: OutputStream => Unit): Unit =
    using(toOutputStream)(outputStream => op(outputStream))
  def usingPrintWriter(op: PrintWriter => Unit): this.type =
    using(toPrintWriter)(printWriter => { op(printWriter); this })
  def writeContent(content: String): this.type = usingPrintWriter(_.print(content))
  def appendContent(content: String) = withAppend.writeContent(content)
  def withAppend: this.type
  def copyFrom(src: InputLocation) = FileUtils.copyFile(src.toFile, toFile)
  def copyFromAsSymlink(src: InputLocation) = Files.createSymbolicLink(toPath, src.toPath)
  def copyFromAsHardLink(src: InputLocation, overwriteIfAlreadyExists: Boolean = false) =
    if (overwriteIfAlreadyExists) {
      Files.createLink(toPath, src.toPath)
    } else {
      if (exists) {
        throw new RuntimeException("Destination file " + this + " already exists.")
      } else {
        Files.createLink(toPath, src.toPath)
      }
    }
}

trait InOutLocation extends InputLocation with OutputLocation {
}

case class FileLocation(fileFullPath: String, append: Boolean = false) extends InOutLocation {
  def raw = fileFullPath
  def asInput: InputLocation = this
  lazy val toFile: File = new File(fileFullPath)
  override def toPath: Path = Paths.get(fileFullPath)
  override def toInputStream: InputStream = new FileInputStream(toFile)
  def child(child: String): this.type = new FileLocation(toPath.resolve(child).toFile.getAbsolutePath).asInstanceOf[this.type]
  def parent: FileLocation.this.type = new FileLocation(parentName).asInstanceOf[FileLocation.this.type]
  def withAppend: this.type = this.copy(append = true).asInstanceOf[this.type]
  def size = toFile.length()
  //import org.raisercostin.util.MimeTypesUtils2
  //def mimeType = MimeTypesUtils2.getMimeType(toPath)
}
case class MemoryLocation(val memoryName: String) extends InputLocation with OutputLocation {
  def raw = memoryName
  def asInput: InputLocation = this
  def append: Boolean = false
  //val buffer: Array[Byte] = Array()
  lazy val outStream = new ByteArrayOutputStream()
  def toFile: File = ???
  override def toOutputStream: OutputStream = outStream
  override def toInputStream: InputStream = new ByteArrayInputStream(outStream.toByteArray())
  def child(child: String): this.type = ???
  def parent: this.type = ???
  def withAppend: this.type = ???
}
object ClassPathInputLocation {
  private def getDefaultClassLoader(): ClassLoader = {
    var cl: ClassLoader = null
    try {
      cl = Thread.currentThread().getContextClassLoader
    } catch {
      case ex: Throwable =>
    }
    if (cl == null) {
      cl = classOf[System].getClassLoader
    }
    cl
  }
  private def getSpecialClassLoader(): ClassLoader =
    //Option(Thread.currentThread().getContextClassLoader).orElse
    (Option(classOf[ClassPathInputLocation].getClassLoader)).orElse(Option(classOf[ClassLoader].getClassLoader)).get
}
/**
 * @see http://www.thinkplexx.com/learn/howto/java/system/java-resource-loading-explained-absolute-and-relative-names-difference-between-classloader-and-class-resource-loading
 */
case class ClassPathInputLocation(initialResourcePath: String) extends InputLocation {
  def raw = initialResourcePath
  import ClassPathInputLocation._
  lazy val resourcePath = initialResourcePath.stripPrefix("/")
  lazy val resource = getSpecialClassLoader.getResource(resourcePath)
  override def toUrl: java.net.URL = resource
  override def exists = resource != null
  override def absolute: String = toUrl.toURI().getPath() //Try{toFile.getAbsolutePath()}.recover{case e:Throwable => Option(toUrl).map(_.toExternalForm).getOrElse("unfound classpath://" + resourcePath) }.get
  def toFile: File = Try { new File(toUrl.toURI()) }.recoverWith { case e: Throwable => Failure(new RuntimeException("Couldn't get file from " + this, e)) }.get
  override def toInputStream: InputStream = getSpecialClassLoader.getResourceAsStream(resourcePath)
  def child(child: String): this.type = new ClassPathInputLocation(resourcePath + FILE_SEPARATOR + child).asInstanceOf[this.type]
  def parent: this.type = new ClassPathInputLocation(parentName).asInstanceOf[this.type]
  ///def toWrite = Locations.file(toFile.getAbsolutePath)
  override def unzip: ZipInputLocation = new ZipInputLocation(this, None)
  override def parentName = {
    val index = initialResourcePath.lastIndexOf("/")
    if (index == -1)
      ""
    else
      initialResourcePath.substring(0, index)
  }
}

case class ZipInputLocation(zip: InputLocation, entry: Option[java.util.zip.ZipEntry]) extends InputLocation {
  def raw = "ZipInputLocation[" + zip + "," + entry + "]"
  def parent: this.type = ???
  def child(child: String): this.type = entry match {
    case None =>
      ZipInputLocation(zip, Some(rootzip.getEntry(child))).asInstanceOf[this.type]
    case Some(entry) =>
      ZipInputLocation(zip, Some(rootzip.getEntry(entry.getName() + "/" + child))).asInstanceOf[this.type]
  }

  def toFile: File = zip.toFile
  override def toInputStream: InputStream = entry match {
    case None =>
      throw new RuntimeException("Can't read stream from zip folder " + this)
    case Some(entry) =>
      rootzip.getInputStream(entry)
  }
  override def list: Iterator[InputLocation] = Option(existing).map(_ => entries).getOrElse(Iterator()).map(entry => ZipInputLocation(zip, Some(entry)))

  private lazy val rootzip = new java.util.zip.ZipFile(toFile)
  import collection.JavaConverters._
  private lazy val entries = rootzip.entries.asScala
}

case class StreamLocation(val inputStream: InputStream) extends InputLocation {
  def raw = "inputStream[" + inputStream + "]"
  def child(child: String): this.type = ???
  def parent: this.type = ???
  def toFile: File = ???
  override def toInputStream: InputStream = inputStream
}
object Locations {
  def classpath(resourcePath: String): ClassPathInputLocation =
    new ClassPathInputLocation(resourcePath)
  def file(fileFullPath: String): FileLocation =
    new FileLocation(fileFullPath)
  def file(path: Path): FileLocation =
    file(path.toFile)
  def file(file: File): FileLocation =
    new FileLocation(file.getAbsolutePath())
  def file(file: File, subFile: String): FileLocation =
    new FileLocation(file.getAbsolutePath()).child(subFile)
  def memory(memoryName: String): MemoryLocation =
    new MemoryLocation(memoryName)
  //io.Source.fromInputStream(getClass.getResourceAsStream(source))
  def apply(s: InputStream): StreamLocation = stream(s)
  def stream(stream: InputStream): StreamLocation = new StreamLocation(stream)
}