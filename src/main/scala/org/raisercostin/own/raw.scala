package org.raisercostin.own

import org.raisercostin.util.io.InputLocation
trait Item {
  def prefix: String
  def tags: Map[String, String]
  def prefixedTags = tags.map(entry => (prefix + "." + entry._1, entry._2))
  def has(tag: String): Seq[(String, String)]
}
case class SimpleItem(prefix: String, location: InputLocation, tags: Map[String, String]) extends Item {
  def has(tag: String): Seq[(String, String)] = tags.filterKeys(_.contains(tag)).toSeq
}
case class CompositeItem(prefix: String, items: Seq[Item]) extends Item {
  lazy val tags = items.reverse.foldLeft(Map[String, String]())((x, item) => x ++ item.tags)
  def has(tag: String): Seq[(String, String)] = items.flatMap { _.has(tag) }
}
object ExifCodes {
  def searchById(id: Int): Option[String] = None
  def searchByAlias(alias: String): Option[String] = None
}

object raw {
  type Extractor = InputLocation => Map[String, String]
  type Itemizer = (InputLocation, Map[String, String]) => Item
  type Analyzer = Map[String, String] => Map[String, String]
  type FullExtractor = InputLocation => Item
  type CompoziteLocator = InputLocation => Seq[InputLocation]

  //def all: (Itemizer*) => Itemizer = items =>CompositeItem("", items.map(x => SimpleItem(x._1, x._2, RichExif.extractExifTags(x._2.toFile).toSimpleMap)))

  val fileAttributesExtractor: Extractor = location => {
    //file properties
    import java.nio.file._
    import java.nio.file.attribute._
    import org.raisercostin.exif.RichExif
    import RichExif._
    val atts = Files.readAttributes(location.toPath, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
    import org.joda.time.DateTime
    val fileAttributes = Map(
      tagFileModificationDateTime -> new DateTime(atts.lastModifiedTime.toString).toString(exifDateTimeFormatter),
      tagFileCreated -> new DateTime(atts.creationTime.toString).toString(exifDateTimeFormatter))
    fileAttributes
  }

  val exifFileNumberAnalyzer: Analyzer = tags =>
    tags.get("exifFileNumber").map(_.toInt).toSeq.flatMap { exifFileNumber =>
      Seq("exifFileNumberMajor" -> "%d".format(exifFileNumber / 10000), "exifFileNumberMinor" -> "%04d".format(exifFileNumber % 10000))
    }.toMap

  val externalExifFullExtractor: FullExtractor = location => simpleItemizer("")(location, externalExifExtractor(location))

  def simpleItemizer(prefix: String): Itemizer = (location, tags) => SimpleItem(prefix, location, tags)

  import com.thebuzzmedia.exiftool.RawExifTool
  import com.thebuzzmedia.exiftool.Feature
  val tool = RawExifTool.Factory.create(Feature.STAY_OPEN, Feature.WINDOWS)
  val externalExifExtractor: Extractor = location => {
    import com.thebuzzmedia.exiftool.ReadOptions
    import scala.collection.JavaConversions._
    tool.getImageMeta(location.toFile, new ReadOptions().withNumericOutput(true)).toMap
  }

  def discoverAdditionalLocations(discoverPairs: Boolean): InputLocation => Seq[(String, InputLocation)] = location => {
    val isMovie = Seq("avi", "mov").contains(location.extension.toLowerCase)
    val thm = if (discoverPairs && isMovie)
      Stream("thm2", "THM", "thm") flatMap { ext => location.withExtension(_ => ext).existingOption.map(x => (ext, x)) } headOption
    else
      None
    val locations = thm ++: Seq(("", location))
    locations
  }

  def externalExifExtractor(discoverPairs: Boolean = true): FullExtractor = location => {
    import org.raisercostin.exif.RichExif
    CompositeItem("", raw.discoverAdditionalLocations(discoverPairs)(location).map(x => simpleItemizer(x._1)(x._2, allAnalyzers(RichExif.extractExifTags(x._2.toFile).toSimpleMap))))
  }
  private def allAnalyzersSeq: Seq[Analyzer] = Seq(exifFileNumberAnalyzer, x => x)
  private def all(analysers: Analyzer*): Analyzer = map => analysers.foldLeft(Map[String, String]())((sum, analyzer) => sum ++ analyzer(map))
  val allAnalyzers:Analyzer = tags => raw.all(allAnalyzersSeq : _*)(tags)
}

trait Extractor {
  def extract(location: InputLocation): Option[Item]
}

object BestExifExtractor extends DecorationExtractor(ExternalExifExtractor(true))

case class ExternalExifExtractor(discoverPairs: Boolean = true) extends Extractor {
  def extract(location: InputLocation): Option[Item] = Some(raw.externalExifExtractor(discoverPairs)(location))
}
object ExternalExifExtractor extends ExternalExifExtractor(true)

case class DecorationExtractor(extractor: Extractor) extends Extractor {
    def extract(location: InputLocation): Option[Item] = 
      extractor.extract(location).map { item => SimpleItem("", location, raw.allAnalyzers(item.tags)) }
  //  def extractFormat(tags: Map[String, String]): Map[String, String] = {
  //    val tags = RichExif.Tags(tags).extractFormat(file, constants2)
  //    result ++= Map(tagFileExtension -> formatted(location.extension)_,
  //      tagCompRemaining -> formatted(cleanFormat(tags))_,
  //      tagCompDetectedFormat -> formatted(tags)_)
}

object FileAttributesExtractor extends Extractor {
  def extract(location: InputLocation): Option[Item] = {
    Some(raw.simpleItemizer("")(location, raw.fileAttributesExtractor(location)))
  }
}

object SanselanExifExtractor extends Extractor {
  def extract(location: InputLocation): Option[Item] = {
    Some(SimpleItem("", location, extract2(location)))
  }

  def extract2(loc: InputLocation): Map[String, String] = {
    val metadata = extractExif(loc)
    import scala.collection.JavaConversions._
    import org.apache.sanselan.Sanselan
    import org.apache.sanselan.formats.jpeg.JpegImageMetadata
    import org.apache.sanselan.formats.tiff.TiffImageMetadata
    import org.apache.sanselan.formats.tiff.constants.ExifTagConstants
    import org.apache.sanselan.formats.tiff.constants.TagInfo
    import org.apache.sanselan.common.ImageMetadata
    var map = Map[String, String]()
    metadata.getItems().foreach {
      case item: TiffImageMetadata.Item =>
        val key = ExifCodes.searchById(item.getTiffField.tagInfo.tag) orElse ExifCodes.searchByAlias(item.getTiffField.tagInfo.name) getOrElse ("notFound-" + item.getTiffField.tagInfo.name.replaceAll("\\s", ""))
        map += key -> item.getText()
      case item: ImageMetadata.Item =>
        val key = ExifCodes.searchByAlias(item.getKeyword()) getOrElse "notFound-" + item.getKeyword().replaceAll("\\s", "")
        map += key -> item.getText()
    }
    map
  }
  private def extractExif(loc: InputLocation) = {
    import org.apache.sanselan.Sanselan
    val metadata = try {
      Sanselan.getMetadata(loc.toFile)
    } catch {
      case e: Throwable => throw new RuntimeException("Can't parse exif for " + loc, e)
    }
    metadata
  }

  //  import org.apache.sanselan.common.{ IImageMetadata, RationalNumber }
  //  import org.apache.sanselan.formats.jpeg.JpegImageMetadata
  //  import org.apache.sanselan.formats.tiff.{ TiffField, TiffImageMetadata }
  //  import org.apache.sanselan.formats.tiff.constants.{ ExifTagConstants, GPSTagConstants, TagInfo, TiffConstants, TiffTagConstants }
  //  import org.apache.sanselan.Sanselan
  //  import org.apache.sanselan.formats.jpeg.JpegImageMetadata
  //  import org.apache.sanselan.formats.tiff.TiffImageMetadata
  //  import org.apache.sanselan.formats.tiff.constants.ExifTagConstants
  //  import org.apache.sanselan.formats.tiff.constants.TagInfo
  //  def extractExifAsMap(file: InputLocation): Tags = {
  //    val metadata = extractExif(file)
  //    extractExifAsMap(metadata)
  //  }
  //  private def extractExifAsMap(metadata: org.apache.sanselan.common.IImageMetadata, extractKeyword: Boolean = true): Tags = {
  //    import scala.collection.JavaConversions._
  //    var map = Map[String, MetadataProvider]()
  //    metadata.getItems().foreach {
  //      case item: TiffImageMetadata.Item =>
  //        if (extractKeyword) {
  //          val value = formatted(item.getTiffField.getValue)_
  //          map += item.getKeyword -> value
  //          map += item.getKeyword.replaceAll("\\s", "") -> value
  //          map += ("E" + item.getTiffField.tagInfo.tag) -> value
  //          val hex = ("0x" + Integer.toHexString(item.getTiffField.tagInfo.tag))
  //          map += ("E" + hex) -> value
  //        }
  //    }
  //    Tags(map)
  //  }

  //  import com.thenewmotion.time.Imports._
  //
  //  private def formatIrfanView(fileName: String, pattern: String): String = {
  //    formatIrfanView(new File(fileName), pattern)
  //  }
  //  private def formatIrfanView(fileName: File, pattern: String): String = {
  //    implicit val metadata = extractExif(fileName)
  //    val date = extractAsDate(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)(metadata)
  //    var result = pattern
  //    val file = Locations.file(fileName)
  //    result = result.replaceAllLiterally("$F", file.name)
  //    //result = result.replaceAll("""\$E36867\(([^)]*)\)""","\\$1")
  //    result = result.replaceAll("""(\$E36867\([^)]*)%Y([^)]*\))""", "$1" + date.toString("yyyy") + "$2")
  //    result = result.replaceAll("""(\$E36867\([^)]*)%m([^)]*\))""", "$1" + date.toString("MM") + "$2")
  //    result = result.replaceAll("""(\$E36867\([^)]*)%d([^)]*\))""", "$1" + date.toString("dd") + "$2")
  //    result = result.replaceAll("""(\$E36867\([^)]*)%H([^)]*\))""", "$1" + date.toString("HH") + "$2")
  //    result = result.replaceAll("""(\$E36867\([^)]*)%M([^)]*\))""", "$1" + date.toString("mm") + "$2")
  //    result = result.replaceAll("""(\$E36867\([^)]*)%S([^)]*\))""", "$1" + date.toString("ss") + "$2")
  //    result = result.replaceAll("""(\$E36867\()([^)]+)\)""", "$2")
  //    result
  //  }
  //  private def extractAsDate(tag: TagInfo)(implicit metadata: org.apache.sanselan.common.IImageMetadata) = {
  //    val data = metadata.asInstanceOf[JpegImageMetadata].findEXIFValue(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)
  //    val date = DateTime.parse(data.getValue.toString.trim, DateTimeFormat.forPattern("yyyy:MM:dd HH:mm:ss"))
  //    date
  //  }
}