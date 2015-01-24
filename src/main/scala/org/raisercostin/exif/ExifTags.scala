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

import org.raisercostin.tags.FormatAnalyser;
import org.raisercostin.util.io.Locations
import org.raisercostin.util.io.InputLocation
import org.raisercostin.tags.raw
import org.raisercostin.tags.Formats
import org.raisercostin.tags.Tags
import org.raisercostin.tags.SimpleTags
import org.raisercostin.util.gps.Gps

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
  def gps():Option[Gps] = tags.getString("exifGPSLatitude").map { x =>
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
