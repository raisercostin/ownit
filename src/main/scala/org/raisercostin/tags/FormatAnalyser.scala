package org.raisercostin.tags

object FormatAnalyser {
  val constants = Seq("IMG","MVI")
  val tagFileModificationDateTime = "fileModification"
  private val dateFormat = "yyyy-MM-dd--HH-mm-ss-ZZ"
  def cleanFormat(format: String) = {
    //this assumes that usually after $ variable a separator might come
    var result = format.replaceAll("""\$\{[^}]+\}[._-]?""", "")
    result = result.replaceAll("^[._-]+", "")
    result = result.replaceAll("[._-]+$", "")
    result
  }
}
case class FormatAnalyser(val tags: Map[String, String]) extends AnyVal {
  import scala.util.{ Try, Success, Failure }

  def apply(pattern: String, constants: Seq[String] = constants): Try[String] = Success(analyse(pattern, constants))
  import org.raisercostin.tags.FormatAnalyser._
  //actual good prefix is this
  //println("a=" + a)
  //println(s"Couldn't find a pattern in [$value]: ${message.reverse.mkString("\n\t", "\n\t", "\n")}")
  //println(s"Couldn't find a date pattern in [$value] is too short. Should have at least 14 characters to match something like yyyyMMddHHmmss.")
  import FormatAnalyser._
  import org.joda.time._

  def analyse(pattern: String, constants: Seq[String] = constants): String = {
    var result = pattern
    var message = List[String]()

      def check(date1: Try[DateTime], prefix: String): Boolean = {
        if (date1.isSuccess) {
          result = extractDateFromString(pattern, date1.get, prefix)
          val mappings = countSubstring(result, prefix)
          if (6 != mappings) {
            //actual good prefix is this
            message ::= s"$prefix = $date1 matched [$result] but have only [$mappings]. 6 are needed."
            false
          } else {
            true
          }
        } else {
          message ::= s"$prefix doesn't exist: " + date1.failed.get.getMessage
          false
        }
      }

      def extractDateTime(tag: String): Try[DateTime] =
        tags.get(tag).map(tag => Formats.extractDate(tag)).getOrElse(Failure(new RuntimeException(s"Tag $tag doesn't exist.")))
    val listDateKeys = Seq("exifDateTimeOriginal", "exifModifyDate", tagFileModificationDateTime, "exifE36867")
    if (pattern.length >= "yyyyMMddHHmmss".length) {
      val dateFields = listDateKeys
      val list = listDateKeys.toStream.map(key => (key, extractDateTime(key))).flatMap {
        case (key, date) =>
          Seq(
            Pair(key, date), Pair(key + "+1s", date.map(_.plusSeconds(1))), Pair(key + "+2s", date.map(_.plusSeconds(2))), Pair(key + "+3s", date.map(_.plusSeconds(3))))
      }
      val stream = list.toStream
      val a = stream.find(x => check(x._2, x._1))
      //println("a=" + a)
      if (a.isEmpty) {
        //println(s"Couldn't find a pattern in [$value]: ${message.reverse.mkString("\n\t", "\n\t", "\n")}")
        result = pattern
      }
    } else {
      //println(s"Couldn't find a date pattern in [$value] is too short. Should have at least 14 characters to match something like yyyyMMddHHmmss.")
    }
    result = extractTagFromString(result, "exifFileNumber")
    result = extractTagFromString(result, "exifFileNumberMajor")
    result = extractTagFromString(result, "exifFileNumberMinor")
    result = extractTagFromString(result, "fileExtension")
    constants.foreach { constant =>
      result = extractConstantFromString(result, constant)
    }
    result
  }

  private def extractDateFromString(text: String, date: DateTime, prefix2: String, suffix: String = "+"): String = {
    var result = text
    val prefix = "$$$$"
    result = replaceFirstLiterally(result, date.toString("yyyy"), "${" + prefix + suffix + "yyyy}")
    result = replaceFirstLiterally(result, date.toString("MM"), "${" + prefix + suffix + "MM}")
    result = replaceFirstLiterally(result, date.toString("dd"), "${" + prefix + suffix + "dd}")
    result = replaceFirstLiterally(result, date.toString("HH"), "${" + prefix + suffix + "HH}")
    result = replaceFirstLiterally(result, date.toString("mm"), "${" + prefix + suffix + "mm}")
    result = replaceFirstLiterally(result, date.toString("ss"), "${" + prefix + suffix + "ss}")
    result = replaceAllLiterally(result, "${" + prefix, "${" + prefix2)
    result
  }
  import java.util.regex.Pattern
  private def countSubstring(str: String, substr: String) = Pattern.quote(substr).r.findAllMatchIn(str).length
  private def extractConstantFromString(text: String, constant: String): String = {
    replaceFirstLiterally(text, constant, "${const:" + constant + "}")
  }

  private def extractTagFromString(text: String, tag: String): String = {
    tags.get(tag) match {
      case Some(value) =>
        //value("") match {
        //case Success(value) =>
        replaceFirstLiterally(text, value, "${" + tag + "}")
      //case Failure(e) =>
      //  throw e
      //}
      case _ =>
        text
    }
  }
  private def replaceFirstLiterally(text: String, literal: String, replacement: String): String = {
    val arg1 = java.util.regex.Pattern.quote(literal)
    val arg2 = java.util.regex.Matcher.quoteReplacement(replacement)
    text.replaceFirst(arg1, arg2)
  }
  private def replaceAllLiterally(text: String, literal: String, replacement: String): String = {
    val arg1 = java.util.regex.Pattern.quote(literal)
    val arg2 = java.util.regex.Matcher.quoteReplacement(replacement)
    text.replaceAll(arg1, arg2)
  }

}
