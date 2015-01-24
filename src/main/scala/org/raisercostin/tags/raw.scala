package org.raisercostin.tags

import scala.util._
import org.raisercostin.tags._
import org.raisercostin.util.io.InputLocation
import org.raisercostin.util.io.Locations
trait Item {
  def prefix: String
  def tags: Map[String, String]
  def prefixedTags = tags.map(entry => (prefix + "." + entry._1, entry._2))
  def has(tag: String): Seq[(String, String)]
}
case class SimpleItem(prefix: String, location: InputLocation, tags: Map[String, String]) extends Item {
  //lazy val tags = tags2.map{case (key,value) => prefix+key -> value}
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
case class Tags(tags: Map[String, String]) {
  private lazy val interpolator = FormatInterpolator(tags)
  private lazy val analyser = FormatAnalyser(tags)
  def apply(tag: String): Option[String] = tags.get(tag)
  def toSimpleMap: Map[String, String] = tags
  def interpolate(pattern: String): Try[String] = interpolator(pattern)
  def analyse(pattern: String): Try[String] = analyser(pattern)

  def getInt(tag: String): Option[Int] = apply(tag).map(_.toInt)
  def getString(tag: String) = apply(tag)
}

object raw {
  object extractor {
    type Extractor = InputLocation => Map[String, String]
    val tagFileExtension = "fileExtension"
    val tagFileCreated = "fileCreated"

    val fileAttributesExtractor: Extractor = location => {

      import java.nio.file._
      import java.nio.file.attribute._
      val atts = Files.readAttributes(location.toPath, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)

      import org.joda.time.DateTime
      val fileAttributes = Map(
        tagFileExtension -> location.extension,
        FormatAnalyser.tagFileModificationDateTime -> new DateTime(atts.lastModifiedTime.toString).toString(Formats.exifDateTimeFormatter),
        tagFileCreated -> new DateTime(atts.creationTime.toString).toString(Formats.exifDateTimeFormatter))
      fileAttributes
    }

    import com.thebuzzmedia.exiftool.RawExifTool
    import com.thebuzzmedia.exiftool.Feature
    private val tool = RawExifTool.Factory.create(Feature.STAY_OPEN, Feature.WINDOWS)
    val externalExifExtractor: Extractor = location => {
      import com.thebuzzmedia.exiftool.ReadOptions

      import scala.collection.JavaConversions._
      tool.getImageMeta(location.toFile, new ReadOptions().withNumericOutput(true)).toMap
    }
    val sanselanExifExtractor: Extractor = location => {
        def extractExif(loc: InputLocation) = {

          import org.apache.sanselan.Sanselan
          val metadata = try {
            Sanselan.getMetadata(loc.toFile)
          } catch {
            case e: Throwable => throw new RuntimeException("Can't parse exif for " + loc, e)
          }
          metadata
        }
      val metadata = extractExif(location)

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
        //case item: TiffImageMetadata.Directory =>
        //  ???
        case x =>
          throw new NotImplementedError(s"Processing of $x not implemented!")
        //        if (extractKeyword) {
        //          val value = formatted(item.getTiffField.getValue)_
        //          map += item.getKeyword -> value
        //          map += item.getKeyword.replaceAll("\\s", "") -> value
        //          map += ("E" + item.getTiffField.tagInfo.tag) -> value
        //          val hex = ("0x" + Integer.toHexString(item.getTiffField.tagInfo.tag))
        //          map += ("E" + hex) -> value

      }
      map
    }
    val bestExtractors: Extractor = location => analysers.prefixKey("" /*file*/ )(fileAttributesExtractor(location)) ++ analysers.prefixKey("exif")(externalExifExtractor(location))
  }

  object analysers {
    type Analyzer = Map[String, String] => Map[String, String]
    private val identityAnalyser: Analyzer = tags => tags
//    private val exifFileNumberAnalyzer: Analyzer = tags =>
//      tags.get("exifFileNumber").map(_.toInt).toSeq.flatMap { exifFileNumber =>
//        Seq("exifFileNumberMajor" -> "%d".format(exifFileNumber / 10000), "exifFileNumberMinor" -> "%04d".format(exifFileNumber % 10000))
//      }.toMap
    val allAnalyzers: Analyzer = all(/*exifFileNumberAnalyzer, */identityAnalyser)
    private def all(analysers: Analyzer*): Analyzer = map => analysers.foldLeft(Map[String, String]())((sum, analyzer) => sum ++ analyzer(map))
    val prefixKey: String => Analyzer = prefix => tags => tags.map { case (key, value) => prefix + key -> value }
  }

  import analysers._
  type Itemizer = (InputLocation, Map[String, String]) => Item
  type FullExtractor = InputLocation => Item

  val externalExifFullExtractor: FullExtractor = location => simpleItemizer("")(location, extractor.bestExtractors(location))

  def simpleItemizer(prefix: String): Itemizer = (location, tags) => SimpleItem(prefix, location, tags)
  val bestExifExtractor: Boolean => InputLocation => Seq[Item] =
    discoverPairs => location => discoverAdditionalLocations(discoverPairs)(location).map(x => simpleItemizer(x._1)(x._2, (extractor.bestExtractors andThen allAnalyzers)(x._2)))
  val all: Boolean => InputLocation => Map[String, String] =
    discoverPairs => location => discoverAdditionalLocations(discoverPairs)(location).reverse.map(x => (x._1, x._2, (extractor.bestExtractors andThen allAnalyzers)(x._2))).foldLeft(Map[String, String]())((x, y) => x ++
      //prefixKey(y._1)(y._3)
      y._3)
  def bestExifFullExtractor: Boolean => InputLocation => Item =
    discoverPairs => location => CompositeItem("", bestExifExtractor(discoverPairs)(location))

  def discoverAdditionalLocations(discoverPairs: Boolean): InputLocation => Seq[(String, InputLocation)] = location => {
    val isMovie = Seq("avi", "mov").contains(location.extension.toLowerCase)
    val thm = if (discoverPairs && isMovie)
      Stream("thm2", "THM", "thm") flatMap { ext => location.withExtension(_ => ext).existingOption.map(x => (ext, x)) } headOption
    else
      None
    val locations = thm ++: Seq(("", location))
    locations
  }
  //  def externalExifExtractor(discoverPairs: Boolean = true): FullExtractor =
  //    location => {
  //      CompositeItem("", raw.discoverAdditionalLocations(discoverPairs)(location).map(x => simpleItemizer(x._1)(x._2, allAnalyzers(RichExif.extractExifTags(x._2.toFile).tags))))
  //    }
  import org.raisercostin.exif.ExifTags
  def loadExifTags(location: InputLocation): ExifTags = ExifTags(new Tags(all(true)(location)))
}
