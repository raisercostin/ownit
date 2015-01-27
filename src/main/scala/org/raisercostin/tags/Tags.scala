package org.raisercostin.tags

import scala.util.Try
import org.joda.time.DateTime
import org.joda.time.LocalDateTime

case class Tags(tags: Map[String, String]) {
  private lazy val interpolator = FormatInterpolator(tags)
  private lazy val analyser = FormatAnalyser(tags)
  def apply(tag: String): Option[String] = tags.get(tag)
  def toSimpleMap: Map[String, String] = tags
  def interpolate(pattern: String): Try[String] = interpolator(pattern)
  def analyse(pattern: String): Try[String] = analyser(pattern)

  def getString(tag: String):Option[String] = apply(tag)
  @deprecated("should return Option[Try[String]] ?")
  def getInt(tag: String): Option[Int] = apply(tag).map(_.toInt)
  @deprecated("should return Option[Try[DateTime]] ?")
  def getDateTime(tag: String):Option[DateTime] = apply(tag).map(Formats.extractDate).map{_.get}
  def asDateTime(value: String):Option[DateTime] = Formats.extractDate(value).toOption
  def asLocalDateTime(value: String):Option[LocalDateTime] = Formats.extractLocalDateTime(value).toOption
}
