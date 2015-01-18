package org.raisercostin.own

object PatternAnalyser {
  val tagFileModificationDateTime = "fileModification"
  private val dateFormat = "yyyy-MM-dd--HH-mm-ss-ZZ"
}
case class PatternAnalyser(tags: Map[String, String]) {
  import scala.util.{ Try, Success, Failure }

  def apply(pattern: String, constants: Seq[String] = Seq("IMG")): Try[String] = Success(analyze(pattern, constants))
  import PatternAnalyser._
  import org.joda.time._

  def analyze(pattern: String, constants: Seq[String] = Seq("IMG")): String = {
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

      def extractDateTime(tag: String):Try[DateTime] = 
        //tags.get(tag).fold[Try[String]] { Failure(new RuntimeException("Not found")) } { (x => Success(x) /*_.apply(dateFormat)*/ ) }.map { (x => DateTime.parse(x, org.joda.time.format.DateTimeFormat.forPattern(dateFormat))) }
        tags.get(tag).map(tag=>raw.Convertor.extractDate(tag)).getOrElse(Failure(new RuntimeException(s"Tag $tag doesn't exist.")))
    val listDateKeys = Seq("exifDateTimeOriginal", "exifModifyDate", tagFileModificationDateTime, "exifE36867")
    //$exifE36867|exifModifyDate|exifDateTimeOriginal
    //implicit val metadata = extractExif(file)
    //val date = extractAsDate(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)
    //println(computeMetadata(metadata).mkString("\n"))
    //val date1 = metadata.get("E36867").flatMap { _.apply(dateFormat) }.map { x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat)) }
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
        replaceFirstLiterally(text, value, "$" + tag + "")
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
