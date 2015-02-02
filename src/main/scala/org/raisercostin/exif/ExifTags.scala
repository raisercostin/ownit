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
}
case class ExifTags(initialTags: Tags) {
  import ExifTags._
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
  def gpsDateTimeUTC = initialTags.getDateTime("exifGPSDateTime")
  //  Track Create Date               : 2014:07:22 17:04:32
  //Track Modify Date               : 2014:07:22 17:04:32
  //Media Create Date               : 2014:07:22 17:04:32
  //Media Modify Date               : 2014:07:22 17:04:32
  def asDateTime(value: Try[String]): Try[DateTime] = value.map(initialTags.asDateTime(_).get)
  def asLocalDateTime(value: Try[String]): Try[LocalDateTime] = value.map(initialTags.asLocalDateTime(_).get)
  def interpolateAsDateTime(value: String): Try[DateTime] = asDateTime(initialTags.interpolate(value + "(" + Formats.dateTimePattern + ")"))
  def interpolateAsLocalDateTime(value: String): Try[LocalDateTime] = asLocalDateTime(initialTags.interpolate(value + "(" + Formats.localDateTimeFormatterISO.name + ")"))
  def dateTimeUTC: Try[DateTime] = {
    val tags = "$exifGPSDateTime|$exifTrackCreateDate|$exifMediaCreateDate"
    interpolateAsDateTime(tags) orElse interpolateAsLocalDateTime(tags).map(_.toDateTime(DateTimeZone.UTC))
  }
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
  val compRemaining: Try[String] = compDetectedFormat.map { format => FormatAnalyser.cleanFormat(format) }

  private def intermediateNewTags: Map[String, String] = Map(
    "dateTime" -> dateTime.map { Formats.toStandard },
    "dateTimeZone" -> dateTimeZone.map { Formats.toStandard },
    "compClosestLocation" -> compClosestLocation, "exifFileNumberMajor" -> compFileNumberMajor.toOption, "exifFileNumberMinor" -> compFileNumberMinor.toOption).collect { case (key, Some(value)) => (key, value) }
  private def finalNewTags: Map[String, String] = Map(
    "compDetectedFormat" -> compDetectedFormat.toOption, "compDetectedPathFormat" -> compDetectedPathFormat.toOption, "compRemaining" -> compRemaining.toOption).collect { case (key, Some(value)) => (key, value) }
  private lazy val intermediateTags: Tags = initialTags.copy(tags = initialTags.tags ++ intermediateNewTags)
  lazy val tags: Tags = intermediateTags.copy(tags = intermediateTags.tags ++ finalNewTags)

  def isImage: Boolean = initialTags.getString("exifMIMEType").getOrElse("").startsWith("image/")
  def isVideo: Boolean = initialTags.getString("exifMIMEType").getOrElse("").startsWith("video/")
}
