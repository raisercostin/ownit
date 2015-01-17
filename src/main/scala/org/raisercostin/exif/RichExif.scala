package org.raisercostin.exif

import com.thebuzzmedia.exiftool.Feature
import com.thenewmotion.time.Imports.DateTime
import com.thenewmotion.time.Imports.DateTimeFormat
import java.io.File
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.attribute.BasicFileAttributes
import java.util.Date
import java.util.GregorianCalendar
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
import org.raisercostin.own.raw
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
    new Gps(GPSLatitude, if (GPSLatitude.toDouble >= 0) "N" else "S", GPSLongitude, if (GPSLongitude.toDouble >= 0) "E" else "W", "0", "0", None)
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
      GPSLongitude = tags.getString("exifGPSLongitude").get //GPSLongitudeRef = tags.getString("exifGPSLongitudeRef").get,
      //GPSAltitude = tags.getString("exifGPSAltitude").getOrElse("0"),
      //GPSAltitudeRef = tags.getString("exifGPSAltitudeRef").getOrElse("0"))
      )
  }
  gps().flatMap { _.closestLocation.name }.map { name =>
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
  lazy val tool = RawExifTool.Factory.create(Feature.STAY_OPEN, Feature.WINDOWS)
  override def close = {
    tool.shutdown()
  }

  type MetadataResult = Try[String]
  type MetadataProvider = (String) => MetadataResult
  type MetadataMapType = Map[String, MetadataProvider]
  type MetadataMapType2 = Map[String, String]
  case class Tags(tags: MetadataMapType) {
    val interpolator = raw.interpolator(toSimpleMap)
    val analyser = raw.analyser(toSimpleMap)
    def getInt(tag: String) = getString(tag).map(_.toInt)
    def getString(tag: String) = tags.get(tag).map(_("").get)
    lazy val toSimpleMap: Map[String, String] =
      tags.mapValues(_("").getOrElse(null)).filter(x => x._2 != null).mapValues(_.toString)
    //see http://dcsobral.blogspot.ro/2010/01/string-interpolation-in-scala-with.html
    def interpolate(pattern: String) = interpolator(pattern)
    def analyze(pattern: String) = analyser(pattern)
    def extractFormat(file: File, constants: Seq[String]): String = analyser(Locations.file(file).baseName, constants).get
    def withTag(value: Pair[String, Any]) = Tags(tags + (value._1 -> formatted(value._2)_))
  }

  private def formatted(value: Any)(format: String): MetadataResult =
    raw.Convertor.formatted(value.toString)(format)

  val tagFileCreated = "fileCreated"
  val tagFileExtension = "fileExtension"
  val tagCompRemaining = "compRemaining"
  val tagCompDetectedFormat = "compDetectedFormat"

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
//  def extractExifTagsInternalToJava(file: File, constants2: Seq[String] = Seq("IMG")): Tags = {
//    SanselanOps.extractExifAsMap(file)
//  }
  def extractExifTags(file: File, constants2: Seq[String] = Seq("IMG")): Tags = {
    //exif metadata
    val exifPrefix = "exif"
    val exifFomFile = Try{extractExifUsingBuzzMedia(exifPrefix, file)}.getOrElse(Map())
    //exif for avi in pair thm file?
    //exif metadata
    val location = Locations.file(file)
    val fileAttributes= raw.fileAttributesExtractor(location)
    val all = exifFomFile ++ (fileAttributes.mapValues(x=>formatted(x)_))
    //println(toSimpleMap(all) mkString "\n")
    //file system data
    var result = all
//    all.get("exifFileNumber").map { exifFileNumber =>
//      exifFileNumber("").get.toInt
//    }.map { exifFileNumber =>
//      result += "exifFileNumberMajor" -> formatted("%d".format(exifFileNumber / 10000))_
//      result += "exifFileNumberMinor" -> formatted("%04d".format(exifFileNumber % 10000))_
//    }
    val tags = Tags(result).extractFormat(file, constants2)
    result ++= Map(tagFileExtension -> formatted(location.extension)_,
      tagCompRemaining -> formatted(cleanFormat(tags))_,
      tagCompDetectedFormat -> formatted(tags)_)
    Tags(TreeMap(result.toSeq: _*))
  }
  private def extractExifUsingBuzzMedia(prefix: String, file: File): MetadataMapType = {
    import scala.collection.JavaConversions._

    val valueMap = tool.getImageMeta(file, new ReadOptions().withNumericOutput(true)).map(x => prefix + x._1 -> formatted(x._2)_)
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
}
