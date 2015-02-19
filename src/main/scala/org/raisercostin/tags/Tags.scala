package org.raisercostin.tags

import scala.util.Try
import org.joda.time.DateTime
import org.joda.time.LocalDateTime
import org.joda.time.DateTimeZone

case class Tags(tags: Map[String, String], formatter: Formatter = Formats) {
  private lazy val interpolator = FormatInterpolator(tags, formatter)
  private lazy val analyser = FormatAnalyser(tags)
  def apply(tag: String): Option[String] = tags.get(tag)
  def interpolate(pattern: String): Try[String] = interpolator(pattern)
  def analyse(pattern: String): Try[String] = analyser(pattern)
  def analyse(pattern: String, excludes: Seq[String]): Try[String] = FormatAnalyser(tags.filterKeys(!excludes.contains(_)))(pattern)

  def getString(tag: String): Option[String] = apply(tag)
  @deprecated("should return Option[Try[String]] ?")
  def getInt(tag: String): Option[Int] = apply(tag).map(_.toInt)
  def getDouble(tag: String): Option[Double] = apply(tag).map(_.toDouble)
  @deprecated("should return Option[Try[DateTime]] ?")
  def getDateTime(tag: String): Option[DateTime] = apply(tag).map(Formats.extractDateTime).map { _.get }
  def getDateTimeZone(tag: String): Option[DateTimeZone] = apply(tag).map(Formats.extractDateTimeZone).map { _.get }
  def getLocalDateTime(tag: String): Option[LocalDateTime] = apply(tag).map(Formats.extractLocalDateTime).map { _.get }

  def asDateTime(value: String): Try[DateTime] = Formats.extractDateTime(value)
  def asDateTimeZone(value: String): Try[DateTimeZone] = Formats.extractDateTimeZone(value)
  def asLocalDateTime(value: String): Try[LocalDateTime] = Formats.extractLocalDateTime(value)

  def cleanFormat(pattern: String): String = FormatAnalyser.cleanFormat(pattern)

  def +(entry: (String, String)) = Tags(tags.updated(entry._1, entry._2), formatter)
  def ++(values: Map[String, String]) = Tags(tags ++ values, formatter)
}
