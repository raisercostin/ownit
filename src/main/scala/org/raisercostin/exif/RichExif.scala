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
import org.raisercostin.own.FormatAnalyser
import org.raisercostin.own.Formats
import org.raisercostin.own.Tags
import org.raisercostin.own.SimpleTags

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
object ExifTags{
  /*
   * To have in ExifTags all methods that are present on Tags and "delegate" to them. 
   * http://jackcoughonsoftware.blogspot.ro/2008/11/using-scala-implicits-to-replace.html
   */
  implicit def delegateToTag(exifTags:ExifTags) = exifTags.tags
}
case class ExifTags(initialTags: Tags){
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
  val compClosestLocation:Option[String] = gps().flatMap { _.closestLocation.name }
  val compDetectedFormat:Try[String] = Try{tags.getString("exifFileName").get}.flatMap(tag=>tags.analyse(tag))
  val compDetectedPathFormat:Try[String] = Try{tags.getString("exifDirectory").get}.flatMap(tag=>tags.analyse(tag))
  val compRemaining:Try[String] = compDetectedFormat.map{format=>FormatAnalyser.cleanFormat(format)}

  def newTags:Map[String,String] = Map("compClosestLocation"->compClosestLocation
      ,"compDetectedFormat"->compDetectedFormat.toOption
      ,"compDetectedPathFormat" -> compDetectedPathFormat.toOption
      ,"compRemaining"->compRemaining.toOption
      ).collect{case (key,Some(value)) => (key,value)}
  
  newTags.map{pair=>
  	tags = tags.withTag(pair)
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
  case class Tags2(tags2: MetadataMapType) extends Tags {
    def apply(tag: String): Option[String] = tags2.get(tag).map(_("").get)
    val interpolator = raw.interpolator(tags)
    val analyser = raw.analyser(tags)
    lazy val tags: Map[String, String] =
      tags2.mapValues(_("").getOrElse(null)).filter(x => x._2 != null).mapValues(_.toString)
    //see http://dcsobral.blogspot.ro/2010/01/string-interpolation-in-scala-with.html
    def interpolate(pattern: String) = interpolator(pattern)
    def analyse(pattern: String) = analyser(pattern)
    def extractFormat(file: File, constants: Seq[String]): String = analyser(Locations.file(file).baseName, constants).get
    def withTag(value: Pair[String, Any]) = Tags2(tags2 + (value._1 -> formatted(value._2)_))
  }

  private def formatted(value: Any)(format: String): MetadataResult =
    Formats.formatted(value.toString)(format)

  val tagFileCreated = "fileCreated"
  val tagFileExtension = "fileExtension"
  val tagCompRemaining = "compRemaining"
  val tagCompDetectedFormat = "compDetectedFormat"
  //  def extractExifTagsInternalToJava(file: File, constants2: Seq[String] = Seq("IMG")): Tags = {
  //    SanselanOps.extractExifAsMap(file)
  //  }
  def extractExifTags(file: File, constants2: Seq[String] = Seq("IMG")): Tags = {
    val exifPrefix = "exif"
    val exifFomFile = Try { extractExifUsingBuzzMedia(exifPrefix, file) }.getOrElse(Map())
    val location = Locations.file(file)
    val fileAttributes = raw.extractor.fileAttributesExtractor(location)
    val all = exifFomFile ++ (fileAttributes.mapValues(x => formatted(x)_))
    var result = all
    val tags = Tags2(result).extractFormat(file, constants2)
    result ++= Map(tagFileExtension -> formatted(location.extension)_,
      tagCompRemaining -> formatted(FormatAnalyser.cleanFormat(tags))_,
      tagCompDetectedFormat -> formatted(tags)_)
    Tags2(TreeMap(result.toSeq: _*))
  }
  private def extractExifUsingBuzzMedia(prefix: String, file: File): MetadataMapType = {
    raw.extractor.externalExifExtractor(Locations.file(file)).map(x => prefix + x._1 -> formatted(x._2)_)
  }
}
