package org.raisercostin.own

import org.raisercostin.util.io.InputLocation
import org.raisercostin.exif.RichExif
import org.apache.sanselan.common.ImageMetadata
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
trait Extractor {
  def extract(location: InputLocation): Seq[Item]
}

object ExifCodes {
  def searchById(id: Int): Option[String] = None
  def searchByAlias(alias: String): Option[String] = None
}

case class ExternalExifExtractor(discoverPairs: Boolean = true) extends Extractor {
  def extract(location: InputLocation): Seq[Item] = {
    val isMovie = Seq("avi", "mov").contains(location.extension.toLowerCase)
    val thm = if (discoverPairs && isMovie)
      Stream("thm2", "THM", "thm") flatMap { ext => location.withExtension(_ => ext).existingOption.map(x => (ext, x)) } headOption
    else
      None
    val locations = thm ++: Seq(("", location))
    Seq(CompositeItem("", locations.map(x => SimpleItem(x._1, x._2, RichExif.extractExifTags(x._2.toFile).toSimpleMap))))
  }
}
object ExternalExifExtractor extends ExternalExifExtractor(true)

object FileAttributesExtractor extends Extractor {
  def extract(location: InputLocation): Seq[Item] = {
    Seq(SimpleItem("", location, extract2(location)))
  }
  def extract2(loc: InputLocation): Map[String, String] = {
    //file properties
    import java.nio.file._
    import java.nio.file.attribute._
    import RichExif._
    val atts = Files.readAttributes(loc.toPath, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
    import org.joda.time.DateTime
    val fileAttributes = Map(
      tagFileModificationDateTime -> new DateTime(atts.lastModifiedTime.toString).toString(exifDateTimeFormatter),
      tagFileCreated -> new DateTime(atts.creationTime.toString).toString(exifDateTimeFormatter))
    fileAttributes
  }
}

object SanselanExifExtractor extends Extractor {
  def extract(location: InputLocation): Seq[Item] = {
    Seq(SimpleItem("", location, extract2(location)))
  }

  def extract2(loc: InputLocation): Map[String, String] = {
    val metadata = extractExif(loc)
    import scala.collection.JavaConversions._
    import org.apache.sanselan.Sanselan
    import org.apache.sanselan.formats.jpeg.JpegImageMetadata
    import org.apache.sanselan.formats.tiff.TiffImageMetadata
    import org.apache.sanselan.formats.tiff.constants.ExifTagConstants
    import org.apache.sanselan.formats.tiff.constants.TagInfo
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
  import org.apache.sanselan.Sanselan
  import org.apache.sanselan.formats.jpeg.JpegImageMetadata
  import org.apache.sanselan.formats.tiff.TiffImageMetadata
  import org.apache.sanselan.formats.tiff.constants.ExifTagConstants
  import org.apache.sanselan.formats.tiff.constants.TagInfo
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