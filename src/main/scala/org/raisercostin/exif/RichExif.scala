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
import org.raisercostin.util.io.InputLocation
case class Distance(meters: Double) {
  def toInternational =
    if (meters >= 1.0)
      f"$meters%.1f km"
    else
      f"${meters / 1000}%.0f m"
}
object Gps {
  //http://download.geonames.org/export/dump/cities1000.zip
  lazy val locations = fromFile(Locations.classpath("cities1000.zip").unzip)
  def fromFile(src: InputLocation): Seq[Gps] = {
    //    0         1            2          3                       4          5    6   7   8       9                10           11         12            13
    //3039154	El Tarter	El Tarter	Ehl Tarter,Эл Тартер	42.57952	1.65362	P	PPL	AD		02				1052		1721	Europe/Andorra	2012-11-03
    src.child("cities1000.txt").readLines.map { line =>
      val fields = line.split("\t")
      Gps(fields(4), "N", fields(5), "E", "12", "0", Some(fields(1)))
    }.toSeq
  }
  def custom = Seq(
    Gps("44.860046", "N", "24.867838", "E", "13.0", "0", Some("pitesti")),
    Gps("44.4378258", "N", "26.0946376", "E", "12", "0", Some("bucuresti")),
    Gps("50.854975", "N", "4.3753899", "E", "12", "0", Some("brussels")))
  def apply(GPSLatitude: String, GPSLongitude: String) = 
    new Gps(GPSLatitude,if(GPSLatitude.toDouble>=0)"N" else "S",GPSLongitude,if(GPSLongitude.toDouble>=0)"E" else "W","0","0",None)
}
//https://www.google.com/maps/place/@44.85597,24.8735028,13z
//https://www.google.com/maps/place/Pite%C8%99ti,+Romania/@44.85597,24.8735028,13z
//https://www.google.com/maps/place/44%C2%B051'21.5%22N+24%C2%B052'24.6%22E/@44.85597,24.8735028,17z/data=!3m1!4b1!4m2!3m1!1s0x0:0x0
//44.860046, 24.867838
//44°51'21.5"N 24°52'24.6"E
case class Gps(GPSLatitude: String, GPSLatitudeRef: String, GPSLongitude: String, GPSLongitudeRef: String, GPSAltitude: String, GPSAltitudeRef: String, name: Option[String] = None) {
  def latitude = GPSLatitude.toDouble
  def longitude = GPSLongitude.toDouble
  def distanceTo(to: Gps) = distance(latitude, longitude, to.latitude, to.longitude)
  private def distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double) = {
    var R = 6371; // km
    var dLat = toRad(lat2 - lat1);
    var dLon = toRad(lon2 - lon1);
    var lat1R = toRad(lat1);
    var lat2R = toRad(lat2);
    var a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.sin(dLon / 2) * Math.sin(dLon / 2) * Math.cos(lat1R) * Math.cos(lat2R);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    var d = R * c;
    Distance(d * 1000)
  }

  private def toRad(value: Double): Double = value * Math.PI / 180

  def mapHref = s"https://www.google.com/maps/@${GPSLatitude},${GPSLongitude},14z"
  //
  //  def distance(position: Position): Option[String] = current map { x => distance(position, x) }
  def closestLocation: Gps = {
    val b = Gps.locations.toIterator.filter(location => near(location.latitude, 10))
    val a = b.minBy(place => distanceTo(place).meters) //map(place => (place, distanceTo(place)))
    //println(a)
    //a.minBy(_._2.meters)._1
    a
  }
  //Each degree of latitude is approximately 69 miles (111 kilometers) apart.
  def near(newLatitude: Double, delta: Double) = (latitude - newLatitude).abs * 111 <= delta
}
case class ExifTags(initialTags: RichExif.Tags) {
  var tags = initialTags
  def fileNumberMinor = tags.getInt("exifFileNumberMinor")
  def fileNumberMajor = tags.getInt("exifFileNumberMajor")
  def fileNumber = tags.getInt("exifFileNumber")
  def gps() = tags.getString("exifGPSLatitude").map { x =>
    Gps(
      GPSLatitude = tags.getString("exifGPSLatitude").get,
      //GPSLatitudeRef = tags.getString("exifGPSLatitudeRef").getOrElse("N"),
      GPSLongitude = tags.getString("exifGPSLongitude").get
      //GPSLongitudeRef = tags.getString("exifGPSLongitudeRef").get,
      //GPSAltitude = tags.getString("exifGPSAltitude").getOrElse("0"),
      //GPSAltitudeRef = tags.getString("exifGPSAltitudeRef").getOrElse("0"))
    )
  }
  gps().flatMap {_.closestLocation.name}.map{name =>
    tags = tags.withTag("compClosestLocation" -> name)
  }
}
object RichExif extends RichExif
class RichExif extends AutoCloseable {
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
  case class Tags(tags: MetadataMapType) {
    def getInt(tag: String) = getString(tag).map(_.toInt)
    def getString(tag: String) = tags.get(tag).map(_("").get)
    def toSimpleMap: Map[String, String] =
      tags.mapValues(_("").getOrElse(null)).filter(x => x._2 != null).mapValues(_.toString)
    //see http://dcsobral.blogspot.ro/2010/01/string-interpolation-in-scala-with.html
    def interpolate(pattern: String) = {
      import scala.util.matching.Regex
      var result = """\$\{([^}]+)\}""".r.replaceAllIn(pattern, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name) => expand(name, tags.get(name), "").getOrElse("")
      })
      result = """\$((?:\w|\|)+)\(([^)]+)\)""".r("name", "expression").replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name, exp) => expandMultiple(name, exp).getOrElse(exp)
      })
      result = """\$(\w+)""".r.replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name) => expand(name, tags.get(name), "").getOrElse("")
      })
      result
    }
    private def expandMultiple(name: String, format: String): Try[String] = {
      val all = name.split("\\|")
      //println(s"""search in ${all mkString "\n"}""")
      name.split("\\|"). //filter{x=> vars.contains(x)}.
        map(x => expand(x, tags.get(x), format)).find(_.isSuccess).headOption.getOrElse(Failure(new RuntimeException("Couldn't find format")))
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

        def extractDateTime(tag: String) = tags.get(tag).fold[Try[String]] { Failure(new RuntimeException("Not found")) } { (_.apply(dateFormat)) }.map { (x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat))) }

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
          Pair("exifDateTimeOriginal", date1), Pair("exifDateTimeOriginal+1s", date1.map(_.plusSeconds(1))),
          Pair("exifDateTimeOriginal+2s", date1.map(_.plusSeconds(2))), Pair("exifDateTimeOriginal+3s", date1.map(_.plusSeconds(3))),
          Pair("exifModifyDate", date3), Pair("exifModifyDate+1s", date3.map(_.plusSeconds(1))),
          Pair("exifModifyDate+2s", date3.map(_.plusSeconds(2))), Pair("exifModifyDate+3s", date3.map(_.plusSeconds(3))),
          Pair("exifE36867", date4), Pair("exifE36867+1s", date4.map(_.plusSeconds(1))), Pair("exifE36867+2s", date4.map(_.plusSeconds(2))),
          Pair("exifE36867+3s", date4.map(_.plusSeconds(3))), Pair("tagFileModificationDateTime", date2),
          Pair("tagFileModificationDateTime+1s", date2.map(_.plusSeconds(1))),
          Pair("tagFileModificationDateTime+2s", date2.map(_.plusSeconds(2))),
          Pair("tagFileModificationDateTime+3s", date2.map(_.plusSeconds(3))))
        val a = stream.find(x => check(x._2, x._1))
        //println("a=" + a)
        if (a.isEmpty) {
          //println(s"Couldn't find a pattern in [$value]: ${message.reverse.mkString("\n\t", "\n\t", "\n")}")
          result = value
        }
      } else {
        //println(s"Couldn't find a date pattern in [$value] is too short. Should have at least 14 characters to match something like yyyyMMddHHmmss.")
      }
      result = extractTagFromString(result, "exifFileNumber")
      result = extractTagFromString(result, "exifFileNumberMajor")
      result = extractTagFromString(result, "exifFileNumberMinor")
      result
    }
    private def extractTagFromString(text: String, tag: String): String = {
      tags.get(tag) match {
        case Some(value) =>
          value("") match {
            case Success(value) =>
              replaceFirstLiterally(text, value, "${" + tag + "}")
            case Failure(e) =>
              throw e
          }
        case _ =>
          text
      }
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
    def withTag(value:Pair[String,Any])=Tags(tags + (value._1 -> formatted(value._2)_))
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
        val exifTry = Try { extractExifAsMap(file).tags.map(x => (prefix + x._1, x._2)) }
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
    var result = fs ++ all
    all.get("exifFileNumber").map { exifFileNumber =>
      exifFileNumber("").get.toInt
    }.map { exifFileNumber =>
      result += "exifFileNumberMajor" -> formatted(exifFileNumber / 10000)_
      result += "exifFileNumberMinor" -> formatted(exifFileNumber % 10000)_
    }
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
