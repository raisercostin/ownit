package org.raisercostin.tags

import scala.util.{ Try, Failure }
import scala.util.Success
/**
 * @see https://www.playframework.com/documentation/2.2.x/ScalaTemplates
 * The formatting string has the syntax:
 * <pre>
 * @{selector}Followed by letters
 * selector(format) - if not followed by letters
 * @{selector(convertor)}
 * selector(convertor)
 * todo: @key1|key2|key3|...|keyN!defaultValue(convertor)
 * $key1|$key2|$key3|...|$keyN|defaultValue(convertor)
 * - the selector
 *     - contains one or multiple keys (prefixed with $) sepparated by | with an optional end default value
 *     - defaultValue can be empty
 *     - if defaultValue is missing and no key is found then an exception will be thrown
 * - format
 *     - is optional (and the default value is %%
 *     - %% - will be replaced by the selected valueuse the value for selected key
 *     - date specific fields %Y %m %d %H %M %S
 *     - any other [a-z][A-Z] characters should be escaped between '
 *     - TODO: time converter conventions: http://joda-time.sourceforge.net/apidocs/org/joda/time/format/DateTimeFormat.html
 * </pre>
 */
//todo use idiomatic matching http://stackoverflow.com/questions/15640555/regex-pattern-matching-with-a-variable-number-of-matching-capturing-groups
case class FormatInterpolator(val tags: Map[String, String]) extends AnyVal {
  import FormatInterpolator._
  def apply(pattern: String): Try[String] = Try {
    import scala.util.matching.Regex
    var result = pattern
    result = """((\$(?:(?:\w|[|$#])+))\(([^)]+)\)(?:[|](\$(?:(?:\w|[|$#])+))\(([^)]+)\))*)""".r.replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
      case all =>
        //println(s"${all.subgroups.size}")
        val pairs = all.subgroups.drop(1).filter(_ != null).grouped(2).toSeq
        //println(s"aici=[${pairs.mkString("\n")}]"); ""
        //expandMultiple(selector, Some(convertor)).map(Regex.quoteReplacement).get //OrElse(convertor)
        val values: Stream[Try[String]] = pairs.toStream.map { case Seq(selector, format) => expandMultiple(selector, Some(format)).map(Regex.quoteReplacement) }
        var cashed = Seq[Try[String]]()
        val firstGood = values.filter { x => if (x.isFailure) cashed = cashed :+ x; x.isSuccess }
        if (firstGood.isEmpty)
          throw new RuntimeException(s"Can't interpolate format [$all]: $cashed")
        firstGood.head.get
      //println()
      //""
    })
    result = """(\$(?:(?:\w|[|$#])+))\(([^)]+)\)""".r.replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
      case Regex.Groups(selector, format) => expandMultiple(selector, Some(format)).map(Regex.quoteReplacement).get //OrElse(convertor)
    })
    result = """(\$(?:(?:\w|[|$#])+))""".r.replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
      case Regex.Groups(selector) => expandMultiple(selector, None).map(Regex.quoteReplacement).get //OrElse("")
    })
    result
  }
  private def expandMultiple(selector: String, convertor: Option[String]): Try[String] = {
    import scala.collection.JavaConversions._
    import com.google.common.base.Splitter
    val all: Iterable[String] = Splitter.on('|').trimResults().split(selector)
    val convertors: List[String] = Splitter.on('|').trimResults().split(convertor.getOrElse("")).toList
    //println(s"expand[$all] with convertors[$convertors]")
    all.toStream.flatMap(extractValue).headOption match {
      case None => Failure(new RuntimeException(s"Couldn't find any value using [$selector]"))
      case Some(value) =>
        Formats.convert(value, convertors.headOption, convertors.drop(1).headOption)
    }
  }
  private def extractValue(name: String): Option[String] =
    if (name.startsWith("$")) tags.get(name.stripPrefix("$")) else Some(name)
}
