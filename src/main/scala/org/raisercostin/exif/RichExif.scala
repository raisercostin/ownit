package org.raisercostin.exif

import com.thebuzzmedia.exiftool.ExifToolService
import com.thebuzzmedia.exiftool.Feature
import com.thenewmotion.time.Imports.DateTime
import com.thenewmotion.time.Imports.DateTimeFormat
import java.io.File
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.attribute.BasicFileAttributes
import java.util.Date
import java.util.GregorianCalendar
import java.util.regex.Pattern
import org.apache.sanselan.Sanselan
import org.apache.sanselan.formats.jpeg.JpegImageMetadata
import org.apache.sanselan.formats.tiff.TiffImageMetadata
import org.apache.sanselan.formats.tiff.constants.ExifTagConstants
import org.apache.sanselan.formats.tiff.constants.TagInfo
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.immutable.TreeMap
import scala.sys.process.Process
import scala.sys.process.ProcessIO
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.matching.Regex
import org.raisercostin.util.io.Locations
case class ExifTags(tags:RichExif.Tags){
  def detectedFormat = tags.vars.get(RichExif.tagCompDetectedFormat).map(_.apply(""))
}
object RichExif extends AutoCloseable {
  import com.thebuzzmedia.exiftool._
  import java.util.regex.Pattern
  import scala.util.Failure
  import java.io.File
  import scala.util.Try
  import scala.util.Success
  import scala.collection.immutable.TreeMap
  lazy val tool = ExifToolService.Factory.create(Feature.STAY_OPEN, Feature.WINDOWS)
  override def close = {
    tool.shutdown()
  }

  type MetadataResult = Try[String]
  type MetadataProvider = (String) => MetadataResult
  type MetadataMapType = Map[String, MetadataProvider]
  case class Tags(vars: MetadataMapType) {
    def toSimpleMap: Map[String, String] =
      vars.mapValues(_("").getOrElse(null)).filter(x => x._2 != null).mapValues(_.toString)
    //see http://dcsobral.blogspot.ro/2010/01/string-interpolation-in-scala-with.html
    def interpolate(pattern: String) = {
      import scala.util.matching.Regex
      var result = """\$\{([^}]+)\}""".r.replaceAllIn(pattern, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name) => expand(name, vars.get(name), "").getOrElse("")
      })
      result = """\$((?:\w|\|)+)\(([^)]+)\)""".r("name", "expression").replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name, exp) => expandMultiple(name, exp).getOrElse(exp)
      })
      result = """\$(\w+)""".r.replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name) => expand(name, vars.get(name), "").getOrElse("")
      })
      result
    }
    private def expandMultiple(name: String, format: String): Try[String] = {
      val all = name.split("\\|")
      //println(s"""search in ${all mkString "\n"}""")
      name.split("\\|"). //filter{x=> vars.contains(x)}.
        map(x => expand(x, vars.get(x), format)).find(_.isSuccess).headOption.getOrElse(Failure(new RuntimeException("Couldn't find format")))
    }

    private def expand(name: String, data: Option[MetadataProvider], format: String): Try[String] = {
      if (data.isEmpty) {
        //println("Couldn't find " + name + " found +" + data)
        Failure(new RuntimeException("Couldn't find " + name + " found +" + data))
      } else {
        val a = data.get.apply(format)
        //println(s"For $name found $a")
        //a.getOrElse(format)
        a
      }
    }
    def extractFormat(file: File): String = {
      analyze(Locations.file(file).baseName)
    }
    
    def analyze(value: String): String = {
      var result = value
      var message = List[String]()

        def check(date1: Try[DateTime], prefix: String): Boolean = {
          if (date1.isSuccess) {
            result = extractDateFromString(value, date1.get, prefix)
            val mappings = countSubstring(result, prefix)
            if (6 != mappings) {
              //actual good prefix is this
              message ::= s"$prefix = $date1 matched [$result] but have only [$mappings]. 6 are needed."
              false
            } else {
              true
            }
          } else {
            message ::= s"$prefix doesn't exist: " + date1.failed.get.getMessage
            false
          }
        }

        def extractDateTime(id: String) = vars.get(id).fold[Try[String]] { Failure(new RuntimeException("Not found")) } { (_.apply(dateFormat)) }.map { (x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat))) }

      //$exifE36867|exifModifyDate|exifDateTimeOriginal
      //implicit val metadata = extractExif(file)
      //val date = extractAsDate(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)
      //println(computeMetadata(metadata).mkString("\n"))
      //val date1 = metadata.get("E36867").flatMap { _.apply(dateFormat) }.map { x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat)) }
      if (value.length >= "yyyyMMddHHmmss".length) {
          lazy val date1: Try[DateTime] = extractDateTime("exifDateTimeOriginal")
          lazy val date3: Try[DateTime] = extractDateTime("exifModifyDate")
          lazy val date2: Try[DateTime] = extractDateTime(tagFileModificationDateTime)
          lazy val date4: Try[DateTime] = extractDateTime("exifE36867")
          val stream = Stream(
            Pair("exifDateTimeOriginal", date1), Pair("exifDateTimeOriginal+1s", date1.map(_.plusSeconds(1))), Pair("exifDateTimeOriginal+2s", date1.map(_.plusSeconds(2))), Pair("exifDateTimeOriginal+3s", date1.map(_.plusSeconds(3))), Pair("exifModifyDate", date3), Pair("exifModifyDate+1s", date3.map(_.plusSeconds(1))), Pair("exifModifyDate+2s", date3.map(_.plusSeconds(2))), Pair("exifModifyDate+3s", date3.map(_.plusSeconds(3))), Pair("exifE36867", date4), Pair("exifE36867+1s", date4.map(_.plusSeconds(1))), Pair("exifE36867+2s", date4.map(_.plusSeconds(2))), Pair("exifE36867+3s", date4.map(_.plusSeconds(3))), Pair("tagFileModificationDateTime", date2), Pair("tagFileModificationDateTime+1s", date2.map(_.plusSeconds(1))), Pair("tagFileModificationDateTime+2s", date2.map(_.plusSeconds(2))), Pair("tagFileModificationDateTime+3s", date2.map(_.plusSeconds(3))))
          val a = stream.find(x => check(x._2, x._1))
          //println("a=" + a)
          if (a.isEmpty) {
            println(s"Couldn't find a pattern in [$value]: ${message.reverse.mkString("\n\t", "\n\t", "\n")}")
            result = value
          }
        } else {
        println(s"Couldn't find a date pattern in [$value] is too short. Should have at least 14 characters to match something like yyyyMMddHHmmss.")
      }
      result
    }
  }
  private def formatted(value: Any)(format: String): MetadataResult =
    //formatted2(value)(format).toOption
    formatted2(value)(format)

  val tagFileModificationDateTime = "fileModification"
  val tagFileCreated = "fileCreated"
  val tagFileExtension = "fileExtension"
  val tagCompRemaining = "compRemaining"
  val tagCompDetectedFormat = "compDetectedFormat"

  private val exifDateTimeFormatter = org.joda.time.format.DateTimeFormat.forPattern("yyyy:MM:dd HH:mm:ss")
  private val exifDateTimeFormatter2 = org.joda.time.format.DateTimeFormat.forPattern("yyyy:MM:dd HH:mm:ssZ")
  private val exifDateTimeFormatter3 = org.joda.time.format.DateTimeFormat.forPattern("yyyy:00:00 00:00:00")
  private val dateFormat = "yyyy-MM-dd--HH-mm-ss-ZZ"

  private def extractExifWithExifTool(prefix: String, file: File): Try[MetadataMapType] =
    Try { extractExifUsingBuzzMedia(prefix, file) }

  private def extractExifWithExifToolOld(prefix: String, file: File): Try[Tags] =
    Try {

        def split(text: String): Pair[String, String] = {
          val all = text.splitAt(text.indexOf(":"))
          Pair(all._1.trim.replaceAll("[/ ]", ""), all._2.drop(1).trim)
        }
      //println("Coulnd't get exif info from " + file)
      import scala.sys.process._
      import scala.sys.process.ProcessIO
      val pb = Process(s"""exiftool "${file.getAbsolutePath}"""")
      var map = Map[String, String]()
      val pio = new ProcessIO(_ => (),
        stdout => scala.io.Source.fromInputStream(stdout)
          .getLines.foreach { x =>
            //println(s"found $x")
            map += split(x)
          },
        _ => ())
      val a = pb.run(pio)
      val blockTillExits = a.exitValue
      if (blockTillExits == 0) {
        //println(map)
        //"exiftool".!
        //println(map mkString "\n")
        val result = map.toMap.map { x =>
          //println(x)
          (prefix + x._1, formatted(x._2)_)
        }
        Tags(result)
      } else {
        throw new RuntimeException(s"Coulnd't get exif info from " + file + ". Got $blockTillExits from exiftool.")
      }
    }

  def extractExifTags(file: File): Tags = {
      def extractExif2(prefix: String, file: File): Try[MetadataMapType] = {
        val exifTry = Try { extractExifAsMap(file).vars.map(x => (prefix + x._1, x._2)) }
        if (exifTry.isFailure) {
          extractExifWithExifTool(prefix, file)
        } else {
          exifTry
        }
      }

    //exif metadata
    val exifPrefix = "exif"
    val thmExifPrefix = "exif"
    val exifPrefix2 = "exif"
    val thmExifPrefix2 = "exif"
    val exifFomFile = extractExif2(exifPrefix, file).getOrElse(Map())
    val exifFomFileWithExifTool: MetadataMapType = extractExifUsingBuzzMedia(exifPrefix2, file)
    val pairThm = if (Locations.file(file).extension.equalsIgnoreCase("avi"))
      Some(Locations.file(file).withExtension(_ => "THM").toFile)
    else
      None
    val exifFromAssociatedThm = pairThm.flatMap(file => extractExif2(thmExifPrefix, file).toOption).getOrElse(Map())
    val exifFromAssociatedThmUsingExifTool = pairThm.map(file => extractExifUsingBuzzMedia(thmExifPrefix2, file)).getOrElse(Map())
    //exif for avi in pair thm file?
    //exif metadata

    //file properties
    import java.nio.file._
    import java.nio.file.attribute._
    val atts = Files.readAttributes(file.toPath, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
    import org.joda.time.DateTime
    val fileAttributes = Map(
      tagFileModificationDateTime -> formatted(new DateTime(atts.lastModifiedTime.toString).toString(exifDateTimeFormatter))_,
      tagFileCreated -> formatted(new DateTime(atts.creationTime.toString).toString(exifDateTimeFormatter))_)
    val all = exifFomFile ++ exifFomFileWithExifTool ++ exifFromAssociatedThm ++ exifFromAssociatedThmUsingExifTool ++ fileAttributes
    //println(toSimpleMap(all) mkString "\n")
    //file system data
    val location = Locations.file(file)
    val tags = Tags(all).extractFormat(file)
    val fs = Map(tagFileExtension -> formatted(location.extension)_,
      tagCompRemaining -> formatted(cleanFormat(tags))_,
      tagCompDetectedFormat -> formatted(tags)_)
    val result = fs ++ all
    Tags(TreeMap(result.toSeq: _*))
  }
  private def extractExifUsingBuzzMedia(prefix: String, file: File): MetadataMapType = {
    import scala.collection.JavaConversions._

    val valueMap = tool.getImageMeta(file).map(x => prefix + x._1.getKey() -> formatted(x._2)_)
    //println(valueMap mkString "\n")
    Map() ++ valueMap
  }

//  private def remainingFormat(file: File) = {
//    var format = extractFormat(file, RichExif.computeMetadata(file))
//  }
//  private def remainingFormat(file: File, metadata: MetadataMap) = {
//    var format = extractFormat(file, metadata)
//    cleanFormat(format)
//  }
  private def cleanFormat(format: String) = {
    //this assumes that usually after $ variable a separator might come
    var result = format.replaceAll("""\$\{[^}]+\}[._-]?""", "")
    result = result.replaceAll("^[._-]+", "")
    result = result.replaceAll("[._-]+$", "")
    result
  }
  type TransformValue = (Any) => String

  private def formatted2(value: Any)(format: String): Try[String] = {
    if (format.isEmpty)
      if (value == null)
        Success("")
      else
        Success(value.toString)
    else {
      //assume is date
      extractDate(value).map { date =>
        val format2 = fromIrfanViewToJodaDateTime(format)
        //println(format2)
        date.toString(format2.replaceAll("%", ""))
      }
      //date
    }
  }
  private def fromIrfanViewToJodaDateTime(format: String) = {
    //%Y-%m-%d--%H-%M-%S
    var result = format
    result = result.replaceAllLiterally("%Y", "yyyy")
    result = result.replaceAllLiterally("%m", "MM")
    result = result.replaceAllLiterally("%d", "dd")
    result = result.replaceAllLiterally("%H", "HH")
    result = result.replaceAllLiterally("%M", "mm")
    result = result.replaceAllLiterally("%S", "ss")
    result
  }
  private def extractDate(value: Any): Try[org.joda.time.DateTime] = {
    import org.joda.time.DateTime
    import org.joda.time.format.DateTimeFormat
    import java.util.GregorianCalendar
    import java.util.Date
    value match {
      case text: String =>
        Try { DateTime.parse(text.trim, exifDateTimeFormatter) }.
          orElse(Try { DateTime.parse(text.trim, exifDateTimeFormatter2) }).
          orElse(Try { DateTime.parse(text.trim, exifDateTimeFormatter3) })
      case date: Date =>
        Try { new DateTime(date) }
      case date: GregorianCalendar =>
        Try { new DateTime(date) }
    }
  }

  import com.thenewmotion.time.Imports._
  import org.apache.sanselan.common.{ IImageMetadata, RationalNumber }
  import org.apache.sanselan.formats.jpeg.JpegImageMetadata
  import org.apache.sanselan.formats.tiff.{ TiffField, TiffImageMetadata }
  import org.apache.sanselan.formats.tiff.constants.{ ExifTagConstants, GPSTagConstants, TagInfo, TiffConstants, TiffTagConstants }

  private def formatIrfanView(fileName: String, pattern: String): String = {
    formatIrfanView(new File(fileName), pattern)
  }
  private def formatIrfanView(fileName: File, pattern: String): String = {
    implicit val metadata = extractExif(fileName)
    val date = extractAsDate(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)
    var result = pattern
    val file = Locations.file(fileName)
    result = result.replaceAllLiterally("$F", file.name)
    //result = result.replaceAll("""\$E36867\(([^)]*)\)""","\\$1")
    result = result.replaceAll("""(\$E36867\([^)]*)%Y([^)]*\))""", "$1" + date.toString("yyyy") + "$2")
    result = result.replaceAll("""(\$E36867\([^)]*)%m([^)]*\))""", "$1" + date.toString("MM") + "$2")
    result = result.replaceAll("""(\$E36867\([^)]*)%d([^)]*\))""", "$1" + date.toString("dd") + "$2")
    result = result.replaceAll("""(\$E36867\([^)]*)%H([^)]*\))""", "$1" + date.toString("HH") + "$2")
    result = result.replaceAll("""(\$E36867\([^)]*)%M([^)]*\))""", "$1" + date.toString("mm") + "$2")
    result = result.replaceAll("""(\$E36867\([^)]*)%S([^)]*\))""", "$1" + date.toString("ss") + "$2")
    result = result.replaceAll("""(\$E36867\()([^)]+)\)""", "$2")
    result
  }
  private def extractExif(fileName: File) = {
    val file = Locations.file(fileName)
    import org.apache.sanselan.Sanselan
    val metadata = try {
      Sanselan.getMetadata(file.toFile)
    } catch {
      case e: Throwable => throw new RuntimeException("Can't parse exif for " + file, e)
    }
    metadata
  }
  private def extractAsDate(tag: TagInfo)(implicit metadata: org.apache.sanselan.common.IImageMetadata) = {
    val data = metadata.asInstanceOf[JpegImageMetadata].findEXIFValue(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)
    val date = DateTime.parse(data.getValue.toString.trim, DateTimeFormat.forPattern("yyyy:MM:dd HH:mm:ss"))
    date
  }
  private def countSubstring(str: String, substr: String) = Pattern.quote(substr).r.findAllMatchIn(str).length
  private def extractDateFromString(text: String, date: DateTime, prefix2: String, suffix: String = "+"): String = {
    var result = text
    val prefix = "$$$$"
    result = replaceFirstLiterally(result, date.toString("yyyy"), "${" + prefix + suffix + "yyyy}")
    result = replaceFirstLiterally(result, date.toString("MM"), "${" + prefix + suffix + "MM}")
    result = replaceFirstLiterally(result, date.toString("dd"), "${" + prefix + suffix + "dd}")
    result = replaceFirstLiterally(result, date.toString("HH"), "${" + prefix + suffix + "HH}")
    result = replaceFirstLiterally(result, date.toString("mm"), "${" + prefix + suffix + "mm}")
    result = replaceFirstLiterally(result, date.toString("ss"), "${" + prefix + suffix + "ss}")
    result = replaceAllLiterally(result, "${" + prefix, "${" + prefix2)
    result
  }
  private def replaceFirstLiterally(text: String, literal: String, replacement: String): String = {
    val arg1 = java.util.regex.Pattern.quote(literal)
    val arg2 = java.util.regex.Matcher.quoteReplacement(replacement)
    text.replaceFirst(arg1, arg2)
  }
  private def replaceAllLiterally(text: String, literal: String, replacement: String): String = {
    val arg1 = java.util.regex.Pattern.quote(literal)
    val arg2 = java.util.regex.Matcher.quoteReplacement(replacement)
    text.replaceAll(arg1, arg2)
  }
  def extractExifAsMap(file: File): Tags = {
    val metadata = extractExif(file)
    extractExifAsMap(metadata)
  }
  private def extractExifAsMap(metadata: org.apache.sanselan.common.IImageMetadata, extractKeyword: Boolean = true): Tags = {
    import scala.collection.JavaConversions._
    var map = Map[String, MetadataProvider]()
    metadata.getItems().foreach {
      case item: TiffImageMetadata.Item =>
        if (extractKeyword) {
          val value = formatted(item.getTiffField.getValue)_
          map += item.getKeyword -> value
          map += item.getKeyword.replaceAll("\\s", "") -> value
          map += ("E" + item.getTiffField.tagInfo.tag) -> value
          val hex = ("0x" + Integer.toHexString(item.getTiffField.tagInfo.tag))
          map += ("E" + hex) -> value
        }
    }
    Tags(map)
  }
}
