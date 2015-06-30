package org.raisercostin.tags

import org.raisercostin.tags._
import com.google.common.cache.CacheBuilder
import com.google.common.cache.CacheLoader
import com.thebuzzmedia.exiftool.adapters.ExifToolService
import java.util.concurrent.TimeUnit
import com.google.common.cache.RemovalListener
import com.google.common.cache.RemovalNotification
import org.joda.time.LocalDateTime
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.DateTimeFormat
import scala.util.Try
import com.thebuzzmedia.exiftool.ExifToolNew3
import org.raisercostin.jedi._
import org.raisercostin.jedi.Locations._
trait Item {
  def prefix: String
  def tags: Map[String, String]
  def prefixedTags = tags.map(entry => (prefix + "." + entry._1, entry._2))
  def has(tag: String): Seq[(String, String)]
}
case class SimpleItem(prefix: String, location: NavigableInputLocation, tags: Map[String, String]) extends Item {
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

object raw {
  object extractor {
    type Extractor = NavigableInputLocation => Map[String, String]
    val tagFileExtension = "fileExtension"
    val tagFileCreated = "fileCreated"

    val fileAttributesExtractor: Extractor = location => {
      import java.nio.file._
      import java.nio.file.attribute._
      val atts = Files.readAttributes(location.toPath, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)

      import org.joda.time.DateTime
      val fileAttributes = Map(
        tagFileExtension -> location.extension,
        FormatAnalyser.tagFileModificationDateTime -> new LocalDateTime(atts.lastModifiedTime.toMillis()).toString(Formats.localDateTimeInternalExifFormatter),
        tagFileCreated -> new LocalDateTime(atts.creationTime.toMillis()).toString(Formats.localDateTimeInternalExifFormatter))
      fileAttributes
    }

    val pathExtractor: Extractor = location => {
      val path = location.name
      val formats = """([12]\d{3})[._\- ]*([01]\d)[._\- ]*([0-3]\d)[._\- ]*([0-6]\d)[._\- ]*([0-6]\d)[._\- ]*([0-6]\d)""".r.unanchored
      val localDateTime = Try {
        path match {
          case formats(year, month, day, hour, minute, second) =>
            implicit def a2i(a: String) = a.toInt
            Some(new LocalDateTime(year, month, day, hour, minute, second))
          case _ => None
        }
      }
      val a = for { x <- localDateTime.toOption; y <- x } yield Map("LocalDateTime" -> y.toString(Formats.localDateTimeInternalExifFormatter))
      a.getOrElse(Map())
    }

    import com.thebuzzmedia.exiftool.RawExifTool
    import com.thebuzzmedia.exiftool.Feature
    private val tool1helper = {
      RawExifTool.Factory.create(Feature.STAY_OPEN, Feature.WINDOWS)
    }
    def tool1: ExifToolService = tool1helper
    /*
    val tool2helper = CacheBuilder.newBuilder().weakValues()
      .expireAfterWrite(20, TimeUnit.SECONDS)
      .removalListener(new RemovalListener[String, ExifToolService]() {
        def onRemoval(removal: RemovalNotification[String, ExifToolService]) = {
          if (removal.getValue() != null)
            removal.getValue().close()
        }
      }).build(new CacheLoader[String, ExifToolService]() {
        def load(key: String): ExifToolService = {
          //println(s"create one more with key $key")
          RawExifTool.Factory.create(Feature.STAY_OPEN, Feature.WINDOWS)
        }
      })
    //def tool2: ExifToolService = tool2helper.get("one")
     */
    def tool: ExifToolService = tool1
    val externalExifExtractor: Extractor = location => {
      import com.thebuzzmedia.exiftool.ReadOptions

      import scala.collection.JavaConversions._
      tool.getImageMeta(location.toFile, new ReadOptions().withNumericOutput(true)).toMap ++ corrected(location)
    }
    def corrected(location: NavigableInputLocation): Map[String, String] = Map("FileName" -> location.name, "Directory" -> location.parent.path)
    //https://github.com/drewnoakes/metadata-extractor
    //val metadataExtractor: Extractor = ???
    val sanselanExifExtractor: Extractor = location => {
      def extractExif(loc: NavigableInputLocation) = {

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
    val bestExtractors: Extractor =
      location =>
        analysers.transformKey(x => x /*file*/ )(fileAttributesExtractor(location)) ++
          analysers.transformKey("exif" + _)(externalExifExtractor(location)) ++
          analysers.transformKey("path" + _)(pathExtractor(location))
  }

  object analysers {
    type Analyzer = Map[String, String] => Map[String, String]
    private val identityAnalyser: Analyzer = tags => tags
    //    private val exifFileNumberAnalyzer: Analyzer = tags =>
    //      tags.get("exifFileNumber").map(_.toInt).toSeq.flatMap { exifFileNumber =>
    //        Seq("exifFileNumberMajor" -> "%d".format(exifFileNumber / 10000), "exifFileNumberMinor" -> "%04d".format(exifFileNumber % 10000))
    //      }.toMap
    val allAnalyzers: Analyzer = all( /*exifFileNumberAnalyzer, */ identityAnalyser)
    private def all(analysers: Analyzer*): Analyzer = map => analysers.foldLeft(Map[String, String]())((sum, analyzer) => sum ++ analyzer(map))
    val transformKey: (String => String) => Analyzer = transformer => tags => tags.map { case (key, value) => transformer(key) -> value }
  }

  import analysers._
  type Itemizer = (NavigableInputLocation, Map[String, String]) => Item
  type FullExtractor = NavigableInputLocation => Item

  private val externalExifFullExtractor: FullExtractor = location => simpleItemizer("")(location, extractor.bestExtractors(location))

  private def simpleItemizer(prefix: String): Itemizer = (location, tags) => SimpleItem(prefix, location, tags)
  private val bestExifExtractor: Boolean => NavigableInputLocation => Seq[Item] =
    discoverPairs => location => discoverAdditionalLocations(discoverPairs)(location).map(x => simpleItemizer(x._1)(x._2, (extractor.bestExtractors andThen allAnalyzers)(x._2)))

  //when multiple maps have the same key, a suffix is added from the received source value otherwise the key is kept
  val all: Boolean => NavigableInputLocation => Map[String, String] =
    discoverPairs => location => {
      val maps = discoverAdditionalLocations(discoverPairs)(location).reverse.map(x => (x._1, x._2, (extractor.bestExtractors andThen allAnalyzers)(x._2)))
      val original = maps.foldLeft(Map[String, String]())((x, y) => x ++
        transformKey(keyTransformer(_, y._1))(y._3))
      maps.foldLeft(Map[String, String]())((x, y) => x ++
        transformKey(key => if (!original.contains(key)) key else keyTransformer(key, y._1))(y._3))
    }
  def keyTransformer(key: String, source: String): String = if (source.isEmpty) key else key + "#" + source
  private def bestExifFullExtractor: Boolean => NavigableInputLocation => Item =
    discoverPairs => location => CompositeItem("", bestExifExtractor(discoverPairs)(location))

  private def discoverAdditionalLocations(discoverPairs: Boolean): NavigableInputLocation => Seq[(String, NavigableInputLocation)] = location => {
    val isMovie = Seq("avi", "mov").contains(location.extension.toLowerCase)
    val thm = if (discoverPairs && isMovie)
      Stream("thm2", "THM", "thm") flatMap { ext => Try { location.withExtension(_ => ext).existingOption }.toOption.flatten.map(x => (ext, x)) } headOption
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
  def loadExifTags(location: NavigableInputLocation): ExifTags = ExifTags(new Tags(all(true)(location)))
}
