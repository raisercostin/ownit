package org.raisercostin.tags

import scala.util.Try

case class Tags(tags: Map[String, String]) {
  private lazy val interpolator = FormatInterpolator(tags)
  private lazy val analyser = FormatAnalyser(tags)
  def apply(tag: String): Option[String] = tags.get(tag)
  def toSimpleMap: Map[String, String] = tags
  def interpolate(pattern: String): Try[String] = interpolator(pattern)
  def analyse(pattern: String): Try[String] = analyser(pattern)

  def getInt(tag: String): Option[Int] = apply(tag).map(_.toInt)
  def getString(tag: String) = apply(tag)
}
