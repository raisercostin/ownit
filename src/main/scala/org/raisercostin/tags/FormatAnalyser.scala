package org.raisercostin.tags

import com.google.common.base.Splitter

object FormatAnalyser {
  val constants = Seq("--+XXXXXXX-- ", "--+XXXX--", "---XXX-IMG_XXXX", "IMG", "MVI", "---at-XXX--XXX", "---at-XXX--", "---at-", "+XXXX", "--XXX", "-XXX", "XXX", "--+XXXX-- ", "DCIM", "-XX-XX", "old-todelete")
  val tagFileModificationDateTime = "fileModification"
  private val dateFormat = "yyyy-MM-dd--HH-mm-ss-ZZ"
  def cleanFormat(format: String) = {
    //this assumes that usually after $ variable a separator might come
    var result = format
    result = result.replaceAll("""\$\{[^}]+\}[._-]?""", "")
    result = result.replaceAll("""\$[a-zA-Z0-9#|]+\([^)]+\)""", "")
    result = result.replaceAll("""\$[a-zA-Z0-9#|]+""", "")
    result = result.replaceAll("^[ ._-]+", "")
    result = result.replaceAll("[ ._-]+$", "")
    result = result.replaceAll("""/[ ._-]+""", "/")
    result = result.replaceAll("""[ ._-]+/""", "/")
    result = result.replaceAll("/+", "/")
    result = result.replaceAll("^/$", "")
    result = result.trim
    result
  }
  //exifFileModifyDate might be wrong but still useful
  //val dateAnalyser = "$dateTime(%Y-%m-%d--%H-%M-%SZ)|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate(%Y-%m-%d--%H-%M-%S'+XXXX')|$exifDateTimeDigitized(%Y-%m-%d--%H-%M-%SZ)|$exifModifyDate#THM|$exifModifyDate(%Y-%m-%d--%H-%M-%S'+XXXX')|$exifFileModifyDate(%Y-%m-%d--%H-%M-%SZ)|(XXXX-XX-XX--XX-XX-XX+XXXX)"
  val dateAnalyser = "$dateTime(%Y-%m-%d--%H-%M-%SZ)|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate(%Y-%m-%d--%H-%M-%S'+XXXX')|$exifDateTimeDigitized(%Y-%m-%d--%H-%M-%SZ)|$exifModifyDate#THM|$exifModifyDate(%Y-%m-%d--%H-%M-%S'+XXXX')|(XXXX-XX-XX--XX-XX-XX+XXXX)"
  //val dateAnalyserNoFormat = "$dateTime|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifModifyDate#THM|$exifModifyDate|$exifFileModifyDate"
  val dateAnalyserNoFormat = "$dateTime|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifModifyDate#THM|$exifModifyDate"
  //val dateAnalyserNoXXXX = "$dateTime(%Y-%m-%d--%H-%M-%SZ)|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifModifyDate#THM|$exifModifyDate(%Y-%m-%d--%H-%M-%S)|$exifFileModifyDate|(%Y-%m-%d--%H-%M-%SZ|)"
  val dateAnalyserNoXXXX = "$dateTime(%Y-%m-%d--%H-%M-%SZ)|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifModifyDate#THM|$exifModifyDate|(%Y-%m-%d--%H-%M-%S|)"
  //val localDateTimeAnalyser = "$dateTime|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifDateTimeDigitized|$exifModifyDate#THM|$exifModifyDate|$exifFileModifyDate(" + Formats.localDateTimeInternalExifFormatterPattern + ")"
  val localDateTimeAnalyser = "$dateTime|$localDateTime|$exifE36867|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifDateTimeDigitized|$exifModifyDate#THM|$exifModifyDate(" + Formats.localDateTimeInternalExifFormatterPattern + ")"
}
protected case class DateTimeResult(result: String, mappings: Int, message: Option[String])
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
    import collection.JavaConverters._
    val a:List[String] = Splitter.on("/").omitEmptyStrings().split(pattern).asScala.toList
    a.map(x=>analyse2(x,constants)).mkString("/")
  }
  private def analyse2(pattern: String, constants: Seq[String]): String = {
    var result = pattern
    val interp = FormatInterpolator(tags)
    result = replaceInterpolated(result, interp, "$exifFileNumberMajor_" + dateAnalyserNoFormat + "(MMdd)")
    result = extractTagFromString(result, "exifFileNumber")
    result = extractTagFromString(result, "exifFileNumberMajor")
    result = extractTagFromString(result, "exifFileNumberMinor")
    result = extractTagFromString(result, "fileExtension")
    result = extractTagFromString(result, "dateTimeZone", Tags(tags).getDateTimeZone("dateTimeZone").map { Formats.toSimplified })
    result = extractDateTags(result)
    result = extractTagFromString(result, "compClosestLocation")
    result = replaceInterpolated(result, interp, dateAnalyserNoFormat+"(yyyy-MM-dd)")
    result = replaceInterpolated(result, interp, dateAnalyserNoFormat+"(yyyy)-XX-XX)")
    constants.zipWithIndex.map {
      case (constant, index) =>
        result = extractConstantFromString(result, constant, index)
    }
    constants.zipWithIndex.map {
      case (constant, index) =>
        result = replaceAllLiterally(result, "${const:" + index + "}", "${const:" + constant + "}")
    }
    //println(result)
    result
  }

  private def extractDateTags(text: String): String = {
    var message = List[String]()
    var result = text

      def check(date1: Try[LocalDateTime], prefix: String): DateTimeResult = {
        if (date1.isSuccess) {
          result = extractDateFromString(text, date1.get, prefix)
          val mappings = countSubstring(result, prefix)
          //actual good prefix is this
          //            message ::= s"$prefix = $date1 matched [$result] but have only [$mappings]. 6 are needed."
          DateTimeResult(result, mappings, Some(s"$prefix = $date1 matched [$result] with [$mappings] mappings."))
        } else {
          //          message ::= s"$prefix doesn't exist: " + date1.failed.get.getMessage
          //          false
          DateTimeResult(result, 0, Some(s"$prefix doesn't exist: " + date1.failed.get.getMessage))
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
    if (text.length >= "yyyy".length) {
      val dateFields = listDateKeys
      val list = listDateKeys.toStream.map(key => (key._1, key._2(key._1))).flatMap {
        case (key, date) =>
          Seq(
            Pair(key, date), Pair(key + "+1s", date.map(_.plusSeconds(1))), Pair(key + "+2s", date.map(_.plusSeconds(2))), Pair(key + "+3s", date.map(_.plusSeconds(3))))
      }
      //(6 to 2).toStream.find { index =>
      val stream = list.toStream
      var max: Option[DateTimeResult] = None
      val a = stream.map(x => check(x._2, x._1)).find { x =>
        if (x.mappings > max.map(_.mappings).getOrElse(-1)) max = Some(x);
        x.mappings == 6
      }
      //println("a=" + a)
      if (a.isEmpty) {
        //println(s"Couldn't find a pattern in [$value]: ${message.reverse.mkString("\n\t", "\n\t", "\n")}")
        //println(max)
        if (max.isDefined && max.get.mappings>=2)
          result = max.get.result
        else
          result = text
      }
      //      a.isDefined
      //}
    } else {
      //println(s"Couldn't find a date pattern in [$value] is too short. Should have at least 14 characters to match something like yyyyMMddHHmmss.")
    }
    result
  }

  private def replaceInterpolated(text: String, interp: FormatInterpolator, format: String): String = {
    val value = interp.apply(format)
    value.map { replaceAllLiterally(text, _, format) }.getOrElse(text)
  }

  private def extractDateFromString(text: String, date: LocalDateTime, prefix2: String, suffix: String = "+"): String = {
    val prefix = "$$$$"

    var result = text
    //var initial = result
    //do {
      //initial = result
      result = replaceAllLiterally(result, date.toString("yyyy"), "${" + prefix + suffix + "yyyy}")
      result = replaceFirstLiterally2(result, date.toString("MM"), "${" + prefix + suffix + "MM}")
      result = replaceFirstLiterally2(result, date.toString("dd"), "${" + prefix + suffix + "dd}")
      result = replaceFirstLiterally2(result, date.toString("HH"), "${" + prefix + suffix + "HH}")
      result = replaceFirstLiterally2(result, date.toString("mm"), "${" + prefix + suffix + "mm}")
      result = replaceFirstLiterally2(result, date.toString("ss"), "${" + prefix + suffix + "ss}")
      if (date.toString("Z").nonEmpty) {
        result = replaceFirstLiterally2(result, date.toString("Z"), "${" + prefix + suffix + "Z}")
      }
    //} while (result != initial)
    result = replaceAllLiterally(result, "${" + prefix, "${" + prefix2)
    result
  }
  import java.util.regex.Pattern
  private def countSubstring(str: String, substr: String) = Pattern.quote(substr).r.findAllMatchIn(str).length
  private def extractConstantFromString(text: String, constant: String, counter: Int): String = {
    replaceAllLiterally(text, constant, "${const:" + counter + "}")
  }
  private def extractTagFromString(text: String, tag: String): String = extractTagFromString(text, tag, tags.get(tag))

  private def extractTagFromString(text: String, tag: String, value: Option[String]): String = value match {
    case Some(value) =>
      //value("") match {
      //case Success(value) =>
      replaceFirstLiterally2(text, value, "${" + tag + "}")
    //case Failure(e) =>
    //  throw e
    //}
    case _ =>
      text
  }
  private def replaceFirstLiterally2(text: String, literal: String, replacement: String): String = {
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
