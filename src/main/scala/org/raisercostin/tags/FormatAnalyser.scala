package org.raisercostin.tags

object FormatAnalyser {
  val constants = Seq("--+XXXXXXX-- ","--+XXXX--","---XXX-IMG_XXXX", "IMG", "MVI", "---at-XXX--XXX", "---at-XXX--", "---at-", "+XXXX", "--XXX", "-XXX","XXX","--+XXXX-- ")
  val tagFileModificationDateTime = "fileModification"
  private val dateFormat = "yyyy-MM-dd--HH-mm-ss-ZZ"
  def cleanFormat(format: String) = {
    //this assumes that usually after $ variable a separator might come
    var result = format
    result = result.replaceAll("""\$\{[^}]+\}[._-]?""", "")
    result = result.replaceAll("""\$[a-zA-Z0-9#|]+\([^)]+\)""", "")
    result = result.replaceAll("""\$[a-zA-Z0-9#|]+""", "")
    result = result.replaceAll("^[._-]+", "")
    result = result.replaceAll("[._-]+$", "")
    result = result.trim
    result
  }
  //exifFileModifyDate might be wrong but still useful
  val dateAnalyser = "$dateTime(%Y-%m-%d--%H-%M-%SZ)|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifModifyDate#THM|$exifModifyDate(%Y-%m-%d--%H-%M-%S'+XXXX')|$exifFileModifyDate(%Y-%m-%d--%H-%M-%SZ)|(XXXX-XX-XX--XX-XX-XX+XXXX)"
  val dateAnalyserNoFormat = "$dateTime|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifModifyDate#THM|$exifModifyDate|$exifFileModifyDate"
  val dateAnalyserNoXXXX = "$dateTime(%Y-%m-%d--%H-%M-%SZ)|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifModifyDate#THM|$exifModifyDate(%Y-%m-%d--%H-%M-%S)|$exifFileModifyDate|(%Y-%m-%d--%H-%M-%SZ|)"
  val localDateTimeAnalyser = "$dateTime|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifModifyDate#THM|$exifModifyDate|$exifFileModifyDate(" + Formats.localDateTimeInternalExifFormatterPattern + ")"
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

  private def analyse(pattern: String, constants: Seq[String]): String = {
    var result = pattern
    val interp = FormatInterpolator(tags)
    result = replaceInterpolated(result, interp, "$exifFileNumberMajor_" + dateAnalyserNoFormat + "(MMdd)")

    var message = List[String]()

      def check(date1: Try[LocalDateTime], prefix: String): Boolean = {
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
      def extractDateTime2(value: Any): Try[LocalDateTime] = Formats.extractDateTime(value).map { _.toLocalDateTime() }
      def extractDateTime[U](format: Any => Try[U])(tag: String): Try[U] =
        tags.get(tag).map(tag => format(tag)).getOrElse(Failure(new RuntimeException(s"Tag $tag doesn't exist.")))
    //exifFileModifyDate could have timezone if not modified?
    //d:\personal\work\ownit\src\test\resources>exiftool 2011-11-21--22-48-54+XXXX---XXX-IMG_XXXX---at-XXX--DSC_0547.JPG |grep Date
    //Bad
    //File Modification Date/Time     : 2011:11:21 23:48:56+02:00
    //File Access Date/Time           : 2015:01:28 21:30:39+02:00
    //File Creation Date/Time         : 2015:01:28 21:30:39+02:00
    //Modify Date                     : 2011:11:21 22:48:54
    //Modify Date                     : 2011:11:21 22:48:54.80
    //Good:
    //Date/Time Original              : 2011:11:19 03:00:20
    //Create Date                     : 2011:11:19 03:00:20
    //Create Date                     : 2011:11:19 03:00:20.80
    //Date/Time Original              : 2011:11:19 03:00:20.80

    val listDateKeys = Seq("dateTime", /*"exifFileModifyDate",*/ "localDateTime", "exifDateTimeOriginal", "exifCreateDate" /*, "exifModifyDate", tagFileModificationDateTime, "exifE36867"*/ ).map(x => (x, extractDateTime(extractDateTime2) _)) ++
      Seq("localDateTime", "localDateTime", "exifDateTimeOriginal", "exifCreateDate", "exifModifyDate").map(x => (x, extractDateTime(Formats.extractLocalDateTime) _))
    if (pattern.length >= "yyyyMMddHHmmss".length) {
      val dateFields = listDateKeys
      val list = listDateKeys.toStream.map(key => (key._1, key._2(key._1))).flatMap {
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
    result = extractTagFromString(result, "dateTimeZone", Tags(tags).getDateTimeZone("dateTimeZone").map { Formats.toSimplified })
    result = extractTagFromString(result, "compClosestLocation")
    constants.zipWithIndex.map {
      case (constant, index) =>
        result = extractConstantFromString(result, constant, index)
    }
    constants.zipWithIndex.map {
      case (constant, index) =>
        result = replaceFirstLiterally(result, "${const:" + index + "}", "${const:" + constant + "}")
    }
    result
  }

  private def replaceInterpolated(text: String, interp: FormatInterpolator, format: String): String = {
    val value = interp.apply(format)
    value.map { replaceFirstLiterally(text, _, format) }.getOrElse(text)
  }

  private def extractDateFromString(text: String, date: LocalDateTime, prefix2: String, suffix: String = "+"): String = {
    var result = text
    val prefix = "$$$$"
    result = replaceFirstLiterally(result, date.toString("yyyy"), "${" + prefix + suffix + "yyyy}")
    result = replaceFirstLiterally(result, date.toString("MM"), "${" + prefix + suffix + "MM}")
    result = replaceFirstLiterally(result, date.toString("dd"), "${" + prefix + suffix + "dd}")
    result = replaceFirstLiterally(result, date.toString("HH"), "${" + prefix + suffix + "HH}")
    result = replaceFirstLiterally(result, date.toString("mm"), "${" + prefix + suffix + "mm}")
    result = replaceFirstLiterally(result, date.toString("ss"), "${" + prefix + suffix + "ss}")
    if (date.toString("Z").nonEmpty) {
      result = replaceFirstLiterally(result, date.toString("Z"), "${" + prefix + suffix + "Z}")
    }
    result = replaceAllLiterally(result, "${" + prefix, "${" + prefix2)
    result
  }
  import java.util.regex.Pattern
  private def countSubstring(str: String, substr: String) = Pattern.quote(substr).r.findAllMatchIn(str).length
  private def extractConstantFromString(text: String, constant: String, counter: Int): String = {
    replaceFirstLiterally(text, constant, "${const:" + counter + "}")
  }
  private def extractTagFromString(text: String, tag: String): String = extractTagFromString(text, tag, tags.get(tag))

  private def extractTagFromString(text: String, tag: String, value: Option[String]): String = value match {
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
