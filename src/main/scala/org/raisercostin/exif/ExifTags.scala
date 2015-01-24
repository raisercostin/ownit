package org.raisercostin.exif

import scala.util.Try

import org.raisercostin.tags.FormatAnalyser
import org.raisercostin.tags.Tags
import org.raisercostin.util.gps.Gps

object ExifTags{
  /*
   * To have in ExifTags all methods that are present on Tags and "delegate" to them. 
   * http://jackcoughonsoftware.blogspot.ro/2008/11/using-scala-implicits-to-replace.html
   */
  implicit def delegateToTag(exifTags:ExifTags) = exifTags.tags
}
case class ExifTags(initialTags: Tags){
  def fileNumberMinor = initialTags.getInt("exifFileNumberMinor")
  def fileNumberMajor = initialTags.getInt("exifFileNumberMajor")
  def fileNumber = initialTags.getInt("exifFileNumber")
  def gpsLatitude = initialTags.getString("exifGPSLatitude")
  def gpsLongitude = initialTags.getString("exifGPSLongitude")
  def gps():Option[Gps] = gpsLatitude.map { x =>
    Gps(
      GPSLatitude = gpsLatitude.get,
      //GPSLatitudeRef = tags.getString("exifGPSLatitudeRef").getOrElse("N"),
      GPSLongitude = gpsLongitude.get
      //GPSLongitudeRef = tags.getString("exifGPSLongitudeRef").get,
      //GPSAltitude = tags.getString("exifGPSAltitude").getOrElse("0"),
      //GPSAltitudeRef = tags.getString("exifGPSAltitudeRef").getOrElse("0"))
      )
  }
  val compClosestLocation:Option[String] = gps().flatMap { _.closestLocation.name }
  val compDetectedFormat:Try[String] = Try{initialTags.getString("exifFileName").get}.flatMap(tag=>initialTags.analyse(tag))
  val compDetectedPathFormat:Try[String] = Try{initialTags.getString("exifDirectory").get}.flatMap(tag=>initialTags.analyse(tag))
  val compRemaining:Try[String] = compDetectedFormat.map{format=>FormatAnalyser.cleanFormat(format)}

  private def newTags:Map[String,String] = Map("compClosestLocation"->compClosestLocation
      ,"compDetectedFormat"->compDetectedFormat.toOption
      ,"compDetectedPathFormat" -> compDetectedPathFormat.toOption
      ,"compRemaining"->compRemaining.toOption
      ).collect{case (key,Some(value)) => (key,value)}
  lazy val tags:Tags = initialTags.copy(tags=initialTags.tags ++ newTags)
}