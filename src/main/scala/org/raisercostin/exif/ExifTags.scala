package org.raisercostin.exif

import scala.util.Try
import org.raisercostin.tags.FormatAnalyser
import org.raisercostin.tags.Tags
import org.raisercostin.util.gps.Gps
import org.raisercostin.tags.Formats
import org.joda.time.DateTime
import org.joda.time.LocalDateTime
import org.joda.time.DateTimeZone
import org.joda.time.Duration
import com.google.common.primitives.Ints
import org.joda.time.format.DateTimeFormatter
import org.raisercostin.tags.Formatter
import org.raisercostin.tags.Validator

object ExifTags {
  /*
   * To have in ExifTags all methods that are present on Tags and "delegate" to them.
   * http://jackcoughonsoftware.blogspot.ro/2008/11/using-scala-implicits-to-replace.html
   */
  implicit def delegateToTag(exifTags: ExifTags) = exifTags.tags
  def computeTimezone(local: LocalDateTime, timeUtc: DateTime): DateTimeZone = {
    val duration = new Duration(timeUtc, local.toDateTime(DateTimeZone.UTC))
    val hours = Ints.checkedCast(duration.getStandardHours())
    if (hours > 25) throw new RuntimeException(s"The difference between $local and $timeUtc is $hours which are more than 25.")
    val minutes = Ints.checkedCast(duration.getStandardMinutes()) - hours * 60
    val minutesRounded = ((minutes + 14) / 30) * 30 //could be 0 : 30 : 60
    if (minutesRounded < 0 || minutesRounded > 60 || minutesRounded % 30 != 0)
      throw new IllegalStateException(s"Minutes rounded from $minutes should be 0, 30 or 60 and it was $minutesRounded.")
    //duration smaller than 25 hours
    //minutes rounded to 0 or 30
    //seconds are ignored
    val totalMinutes = hours * 60 + minutesRounded
    DateTimeZone.forOffsetMillis(totalMinutes * 60 * 1000)
    //DateTimeZone.forOffsetHoursMinutes(hours,minutesRounded)
  }
  case class Version(date: DateTime, version: String)
  val versions = """|
 | @see http://www.photometadata.org/meta-resources-metadata-types-standards-exif
 | History of the Exif standard
 | Date	Version	Description
 | October 1995	1	Established as a JEIDA standard. Defined the structure, consisting of an image data format and attribute information (tags), and basic tags.
 | November 1997	1.1	Kept the essential provisions of Version 1.0 and added provisions for optional attribute information and format operation
 | June 1998	2	Added sRGB color space, compressed thumbnails and audio files
 | December 1998	2.1	Upgraded and expanded the storage format and attribute information. Added recommended compatibility details as a supplement to Version 2.0
 | February 2002	2.2	Added information to Version 2.1 to improve print finishing
 | September 2003	2.21	Added optional color space (Adobe RGB)
""".stripMargin.split("\n").toList.drop(4).map { _.split("\t").toSeq }.map {
    case Seq(date, version, description) =>
      val versionInt = (version.toDouble * 100).toInt
      Version(Formats.parseLocalDateTime("MMMM yyyy", date.trim).get.toDateTime(DateTimeZone.UTC), String.format("%04d", versionInt.asInstanceOf[Object]))
  }.sortBy(_.version)
  //println(versions.mkString("\n"))
  def availabilityDate(version: String): Option[DateTime] = versions.filter(x => x.version <= version).lastOption.map(_.date)
}
case class ExifTags(rootInitialTags: Tags) {
  import ExifTags._
  val formatter = new Formats(Some(new Validator {
    def validateDateTime(dateTime: DateTime): Option[DateTime] = validDateTime(dateTime)
    def validateLocalDateTime(dateTime: LocalDateTime): Option[LocalDateTime] = validLocalDateTime(dateTime)
  }))
  val initialTags = rootInitialTags.copy(formatter = formatter)
  def deviceId: Option[String] = Some(initialTags.interpolate("exifModel:$exifModel|-exifCanonModelId:$exifCanonModelID|-exifProfileID:$exifProfileID|-exifDeviceModelDesc:$exifDeviceModelDesc|-exifDeviceModel:$exifDeviceModel|").get)
  lazy val localDateTime: Option[LocalDateTime] = initialTags.interpolate(
    FormatAnalyser.localDateTimeAnalyser).toOption.flatMap(x => initialTags.asLocalDateTime(x).toOption)
  //FormatAnalyser.dateAnalyser
  lazy val dateTimeZone: Option[DateTimeZone] = for (x <- localDateTime; y <- dateTimeUTC.toOption) yield computeTimezone(x, y)
  lazy val dateTime: Option[DateTime] = for (x <- localDateTime; y <- dateTimeZone) yield x.toDateTime(y)
  //exifFileModifyDate could have timezone if not modified?
  def fileExtension = initialTags.getString("fileExtension")
  def fileNumber = initialTags.getInt("exifFileNumber")
  def gpsLatitude = initialTags.getString("exifGPSLatitude")
  def gpsLongitude = initialTags.getString("exifGPSLongitude")
  def gpsDateTimeUTC = getDateTime("exifGPSDateTime")
  //  Track Create Date               : 2014:07:22 17:04:32
  //Track Modify Date               : 2014:07:22 17:04:32
  //Media Create Date               : 2014:07:22 17:04:32
  //Media Modify Date               : 2014:07:22 17:04:32
  def dateTimeUTC: Try[DateTime] = {
    val tagsLocal = "$exifGPSDateTime|$exifTrackCreateDate|$exifMediaCreateDate"
    val tags = tagsLocal + "|$exifDateTimeOriginal"
    interpolateAsDateTime(tags).map { _.withZone(DateTimeZone.UTC) } orElse interpolateAsLocalDateTime(tagsLocal).map(_.toDateTime(DateTimeZone.UTC))
  }
  val dateGroup = "$dateTime|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifDateTimeDigitized|$exifModifyDate#THM|$exifModifyDate|$exifFileModifyDate"
  val dateGroup2 = "$dateTime|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifDateTimeDigitized|$exifModifyDate#THM|$exifModifyDate"
  def gpsDateTime = for (x <- gpsDateTimeUTC; y <- dateTimeZone) yield x.withZone(y)
  def gps(): Option[Gps] = gpsLatitude.map { x =>
    Gps(
      GPSLatitude = gpsLatitude.get,
      //GPSLatitudeRef = tags.getString("exifGPSLatitudeRef").getOrElse("N"),
      GPSLongitude = gpsLongitude.get //GPSLongitudeRef = tags.getString("exifGPSLongitudeRef").get,
    //GPSAltitude = tags.getString("exifGPSAltitude").getOrElse("0"),
    //GPSAltitudeRef = tags.getString("exifGPSAltitudeRef").getOrElse("0"))
    )
  }
  val compClosestLocation: Option[String] = gps().flatMap { _.closestLocation.name }
  private val compFileNumberMajor: Try[String] = Try { fileNumber.get }.map(exifFileNumber => "%d".format(exifFileNumber / 10000))
  private val compFileNumberMinor: Try[String] = Try { fileNumber.get }.map(exifFileNumber => "%04d".format(exifFileNumber % 10000))
  def fileNumberMajor = compFileNumberMajor.map(_.toInt)
  def fileNumberMinor = compFileNumberMinor.map(_.toInt)
  val compDetectedFormat: Try[String] = Try { initialTags.getString("exifFileName").get }.flatMap(tag => intermediateTags.analyse(tag))
  val compDetectedPathFormat: Try[String] = Try { initialTags.getString("exifDirectory").get }.flatMap(tag => intermediateTags.analyse(tag))
  val compRemaining: Try[Option[String]] = compDetectedFormat.map { format => Option(FormatAnalyser.cleanFormat(format)).filter(_.trim.nonEmpty) }

  private def intermediateNewTags: Map[String, String] = Map(
    "dateTime" -> dateTime.map { Formats.toStandard },
    "dateTimeZone" -> dateTimeZone.map { Formats.toStandard },
    "compClosestLocation" -> compClosestLocation, "exifFileNumberMajor" -> compFileNumberMajor.toOption, "exifFileNumberMinor" -> compFileNumberMinor.toOption).collect { case (key, Some(value)) => (key, value) }
  private def finalNewTags: Map[String, String] = Map(
    "compDetectedFormat" -> compDetectedFormat.toOption, "compDetectedPathFormat" -> compDetectedPathFormat.toOption, "compRemaining" -> compRemaining.toOption.flatten).collect { case (key, Some(value)) => (key, value) }
  private lazy val intermediateTags: Tags = initialTags.copy(tags = initialTags.tags ++ intermediateNewTags)
  lazy val tags: Tags = intermediateTags.copy(tags = intermediateTags.tags ++ finalNewTags)

  def isImage: Boolean = initialTags.getString("exifMIMEType").getOrElse("").startsWith("image/")
  def isVideo: Boolean = initialTags.getString("exifMIMEType").getOrElse("").startsWith("video/")

  def hasExif: Boolean = initialTags.getString("exifError").isEmpty
  def exifVersion: Option[String] = initialTags.getString("exifExifVersion")
  def exifVersionDate: Option[DateTime] = exifVersion.flatMap { version => availabilityDate(version) }
  def originalExif: Boolean = initialTags.getString("exifModel").isDefined
  def getDateTime(tag: String): Option[DateTime] = intermediateTags.getDateTime(tag)
  def getLocalDateTime(tag: String): Option[LocalDateTime] = intermediateTags.getLocalDateTime(tag)
  //  def getDateTime(tag: String):Option[DateTime] = validate(initialTags.getDateTime(tag))
  //  def validate(date:Option[DateTime]):Option[DateTime] = date.flatMap{validDateTime}
  def validDateTime(date: DateTime): Option[DateTime] = exifVersionDate match {
    case None =>
      Some(date)
    case Some(first) if !first.isAfter(date) =>
      Some(date)
    case _ =>
      None
  }
  //  def getLocalDateTime(tag: String):Option[LocalDateTime] = validate2(initialTags.getLocalDateTime(tag))
  //  def validate2(date:Option[LocalDateTime]):Option[LocalDateTime] = date.flatMap{validLocalDateTime}
  def validLocalDateTime(date: LocalDateTime): Option[LocalDateTime] = exifVersionDate match {
    case None =>
      Some(date)
    case Some(first) if !first.toLocalDateTime().isAfter(date) =>
      Some(date)
    case _ =>
      None
  }
  //  {val a = for{first <- exifVersionDate if !first.toLocalDateTime().isAfter(date)} yield date
  //    a.orElse(Some(date))
  //  }

  def asDateTime(value: Try[String]): Try[DateTime] = value.map(initialTags.asDateTime(_).get) //.map{x=>validDateTime(x).get}
  def asLocalDateTime(value: Try[String]): Try[LocalDateTime] = value.map(initialTags.asLocalDateTime(_).get) //.map{x=>validLocalDateTime(x).get}
  def interpolateAsDateTime(value: String): Try[DateTime] = asDateTime(initialTags.interpolate(value + "(" + Formats.dateTimePattern + ")"))
  def interpolateAsLocalDateTime(value: String): Try[LocalDateTime] = asLocalDateTime(initialTags.interpolate(value + "(" + Formats.localDateTimeFormatterISO.name + ")"))
}
