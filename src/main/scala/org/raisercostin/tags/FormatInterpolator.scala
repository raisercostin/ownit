package org.raisercostin.tags

import scala.util.{ Try, Failure }
import scala.util.Success
import org.junit.Assert
import scala.util.parsing.combinator.RegexParsers
import scala.util.Failure
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

//object InterpolatorParser extends RegexParsers {
//  def word:Parser[String] = """\$(\w(?:\w|[#])*)""".r 
//  def words:Parser[Seq[String]] = word ~ opt("|" ~ word) ^^ {case word1 ~ opt("|" ~ word2) => Seq(word1,word2)}
//  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
//  def factor: Parser[Double] = number | "(" ~> expr <~ ")"
//  def term: Parser[Double] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
//    case number ~ list => (number /: list) {
//      case (x, "*" ~ y) => x * y
//      case (x, "/" ~ y) => x / y
//    }
//  }
//  def expr: Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
//    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
//      case (x, "+" ~ y) => x + y
//      case (x, "-" ~ y) => x - y
//    }
//  }
//
//  def apply(input: String): Try[Seq[String]] = parseAll(words, input) match {
//    case Success(result, _) => scala.util.Success(result)
//    case failure: NoSuccess => scala.util.Failure(new RuntimeException(failure.toString()))
//  }
//}
object FormatInterpolator{
    //    val matcher = java.util.regex.Pattern.compile("(?:(\\w)-?)+").matcher("a-b-c-d-e-f")
    //    matcher.matches()
    //    println(matcher.groupCount())
    //    println(matcher.group(0))
    //    println(matcher.group(1))
//    val groupOfVariables = """((?:\w|[$#|])*)"""
//    val groupOfFormatters = """\(([^)]+)\)"""
//    val pattern1 = s"$groupOfVariables$groupOfFormatters[|]?"
    //println(pattern1)
    val variable = """(?:\$\w(?:\w|[#])*)"""
    val constant = """(?:\w*)"""
    val variables = s"""$variable(?:[|]$variable)*"""
    val selector = s"""(?:$variables)"""//s"""($variables(?:[|]$constant)?)"""
    val format = """(?:\(([^)]+)\))"""
    val section = s"""(?:$selector$format?)"""
    val finalSection = s"""(?:[|](?:(?:$constant$format)|$constant|$format))?"""
    val sections = s"""$section(?:[|]$section)*$finalSection"""

    val nonEmptyConstant = """(?:\w+)"""
    val fullSelector = s"""(?:[^(]+)"""
    val fullSelectors= s"""$fullSelector(?:[|]$fullSelector?)*"""
    val fullSection = s"""[|]?($fullSelectors?)$format?"""
    //val fullSection = s"""(?:[|]?([^(]+)$format?)|(?:[|]$format)"""
    
//    val word = variable
//    val simpleWord = """(\w(\w|[#])*)"""
    //val patternAll = """(\$\w(\w|[#])*)(\|(\$\w(\w|[#])*))*\|?(\([^)]+\))?"""
    //val patternAll2 = s"""$word(\\|$word)*(\\|$simpleWord)?$format?"""
    val sectionsRegex = sections.r
    val sectionRegex = fullSection.r
//    println(s"sections=$sections")
//    println(s"fullSelector=$fullSelector")
//    println(s"fullSection=$fullSection")
}
case class FormatInterpolator(val tags: Map[String, String]) extends AnyVal {
  import FormatInterpolator._
  def apply(pattern: String): Try[String] = Try {

    import scala.util.matching.Regex
    //Assert.assertEquals(patternAll,patternAll2)
    var result = pattern
    //"""(\$\w(?:(?:(?:\w|[$|#])|(?:\([^)]+\)))*))"""
    result = sectionsRegex.replaceAllIn(result, _ match {
      //result = """(\$(?:(?:\w|[|$#])+))\(([^)]+)\)(?:[|](\$(?:(?:\w|[|$#])+))\(([^)]+)\))*""".r.replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
      case all =>
        //println(s"matched[${all.matched}]")
        val matcher = sectionRegex findAllIn all.matched
        val all2 = matcher map { _ => (matcher.group(1).stripPrefix("|"), Option(matcher.group(2))) }
        expandMultipleSelectors(all2.toList).get
      //        } else {
      //          println(all.matched)
      //          all.matched
      //          val simple = """(\$(?:(?:\w|[|$#])+))\(([^)]+)\)""".r
      //          val simple2 = """(\$(?:(?:\w|[|$#])+))""".r
      //          all.matched match {
      //            case simple(selector, format) => expandMultiple(selector, Option(format)).map(Regex.quoteReplacement).get //OrElse(convertor)
      //            case simple2(selector) => expandMultiple(selector, None).map(Regex.quoteReplacement).get //OrElse("")
      //          }
      //        }
    })
    //    result = """(\$(?:(?:\w|[|$#])+))\(([^)]+)\)""".r.replaceAllIn(result, _ match {
    //      case Regex.Groups(selector, format) => expandMultiple(selector, Option(format)).map(Regex.quoteReplacement).get //OrElse(convertor)
    //    })
    //    result = """(\$(?:(?:\w|[|$#])+))""".r.replaceAllIn(result, _ match {
    //      case Regex.Groups(selector) => expandMultiple(selector, None).map(Regex.quoteReplacement).get //OrElse("")
    //    })
    result
  }
  private def expandMultipleSelectors(all: List[(String, Option[String])]): Try[String] = {
    //println(all.mkString("formatters\n\t", "\n\t", "\n"))
    val values: Stream[Try[String]] = all.toStream.map { case (selector, format) => expandMultiple(selector, format).map(scala.util.matching.Regex.quoteReplacement) }
    var cashed = Seq[Try[String]]()
    val firstGood = values.filter { x => if (x.isFailure) cashed = cashed :+ x; x.isSuccess }
    if (firstGood.isEmpty)
      throw new RuntimeException(s"Can't interpolate format [$all]: $cashed")
    firstGood.head
  }
  private def expandMultiple(selector: String, convertor: Option[String]): Try[String] = {
    import scala.collection.JavaConversions._
    import com.google.common.base.Splitter
    val all: Iterable[String] = if(selector.isEmpty()) Seq("") else Splitter.on('|').split(selector)
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
