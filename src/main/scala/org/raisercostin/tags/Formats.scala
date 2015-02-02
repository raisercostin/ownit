package org.raisercostin.tags

import java.util.Date
import java.util.GregorianCalendar
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.joda.time.DateTime
import org.joda.time.LocalDateTime
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.DateTimeZone
object Formats {
  import scala.util.{ Try, Failure, Success }
  def convert(value: String, convertor: Option[String] = None, convertorNull: Option[String] = None): Try[String] =
    if (value.isEmpty)
      if (convertorNull.isEmpty)
        if (convertor.isEmpty)
          Success("")
        else
          formatted("")(convertor.get)
      else
        Success(convertorNull.get)
    else if (convertor.isEmpty)
      Success(value)
    else
      formatted(value)(convertor.get)

  def formatted(value: String)(format: String): Try[String] =
    if (format.isEmpty)
      if (value == null)
        Success("")
      else
        Success(value.toString)
    else
      ((simpleConverter orElse dateTimeConverter)((format, value))).orElse(localDateTimeConverter(format, value)).orElse(useFormatString(format,value))

  def simpleConverter: PartialFunction[(String, String), Try[String]] = {
    case (format, value) if format.contains("%%") => Success(format.replaceAllLiterally("%%", value))
  }
  def dateTimeConverter: PartialFunction[(String, String), Try[String]] = {
    case (format, value) =>
      //println(s"convert[$format][$value]")
      //assume is date
      extractDateTime(value).map { date =>
        val format2 = fromIrfanViewToJodaDateTime(format)
        date.toString(format2.replaceAll("%", ""))
      } match {
        case Success(s) => Success(s)
        case Failure(ex) => Failure(new RuntimeException(s"Couldn't format date with [$format] " + ex.getMessage(), ex))
      }
  }
  def localDateTimeConverter: PartialFunction[(String, String), Try[String]] = {
    case (format, value) =>
      //println(s"convert[$format][$value]")
      //assume is date
      extractLocalDateTime(value).map { date =>
        val format2 = fromIrfanViewToJodaDateTime(format)
        date.toString(format2.replaceAll("%", ""))
      } match {
        case Success(s) => Success(s)
        case Failure(ex) => Failure(new RuntimeException(s"Couldn't format date with [$format] " + ex.getMessage(), ex))
      }
  }
  def useFormatString: PartialFunction[(String, String), Try[String]] = {
    case (format, "") =>
      Success(format)
  }
/*
  case class DateTimeFormatWithPattern(val pattern: String) {
    lazy val formatter: DateTimeFormatter = DateTimeFormat.forPattern(pattern)
    def withOffsetParsed = DateTimeFormatWithPattern(pattern)
  }
  object DateTimeFormatWithPattern {
    implicit def toDateTimeFormatter(format: DateTimeFormatWithPattern) = format.formatter
  }
*/

  val localDateTimeInternalExifFormatterPattern = "yyyy:MM:dd HH:mm:ss"
  val localDateTimeInternalExifFormatter = DateTimeFormat.forPattern(localDateTimeInternalExifFormatterPattern)
  val localDateTimeFormatterISO = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
  def toStandard(value: LocalDateTime): String = value.toString(localDateTimeFormatterISO)

  val dateTimeInternalExifFormatter = DateTimeFormat.forPattern("yyyy:MM:dd HH:mm:ssZ").withOffsetParsed()
  val dateTimeInternalExifFormatter2 = DateTimeFormat.forPattern("yyyy:MM:dd HH:mm:ss.SSSZ").withOffsetParsed()
  private val exifDateTimeFormatter3 = DateTimeFormat.forPattern("yyyy:00:00 00:00:00")
  private val dateTimeIsoFormatter = ISODateTimeFormat.dateTime().withOffsetParsed()
  def toStandard(value: DateTime): String = value.toString(dateTimeIsoFormatter)
  def toStandard(value: DateTimeZone): String = value.toString

  //def extractLocalDateTime(value: String): Try[LocalDateTime] = Try { LocalDateTime.parse(value, localDateTimeInternalExifFormatter.withOffsetParsed()) }
  def extractLocalDateTime(value: Any): Try[LocalDateTime] = {
    import org.joda.time.DateTime
    import org.joda.time.format.DateTimeFormat
    import java.util.GregorianCalendar
    import java.util.Date
    value match {
      case text: String =>
        val formatters = Seq(localDateTimeInternalExifFormatter, localDateTimeFormatterISO)
          def parse(value: DateTimeFormatter) = Try { LocalDateTime.parse(text.trim, value) }
        parseInOrder("LocalDateTime", formatters, parse, text.trim)
      case date: Date =>
        Try { new LocalDateTime(date) }
      case date: GregorianCalendar =>
        Try { new LocalDateTime(date) }
      case x =>
        throw new IllegalArgumentException(s"Can't convert [$value] of type [${value.getClass}] to LocalDateTime.")
    }
  }

  def extractDateTime(value: Any): Try[org.joda.time.DateTime] = {
    import org.joda.time.DateTime
    import org.joda.time.format.DateTimeFormat
    import java.util.GregorianCalendar
    import java.util.Date
    value match {
      case text: String =>
        val formatters = Seq(dateTimeIsoFormatter, dateTimeInternalExifFormatter2, dateTimeInternalExifFormatter, exifDateTimeFormatter3)
          def parse(value: DateTimeFormatter) = Try { DateTime.parse(text.trim, value) }
        parseInOrder("DateTime", formatters, parse, text.trim)
      case date: Date =>
        Try { new DateTime(date) }
      case date: GregorianCalendar =>
        Try { new DateTime(date) }
      case x =>
        throw new IllegalArgumentException(s"Can't convert [$value] of type [${value.getClass}] to DateTime.")
    }
  }

  private def failureDetail[U](detail: String, value: String): Throwable => Try[U] = f => Failure[U](new RuntimeException(s"Couldn't extract a ${detail} from value [$value] using formatters.", f))
  private def parseInOrder[T, U](detail: String, formatters: Seq[T], parse: T => Try[U], value: String): Try[U] =
    formatters.drop(1).foldLeft(parse(formatters.head))((prev, formatter) => prev.orElse(parse(formatter)))
      .
      transform(
        x => Success(x),
        failureDetail(detail, value))

  private def fromIrfanViewToJodaDateTime(format: String) = {
    //%Y-%m-%d--%H-%M-%S
    var result = format
    result = result.replaceAllLiterally("%Y", "yyyy")
    result = result.replaceAllLiterally("%m", "MM")
    result = result.replaceAllLiterally("%d", "dd")
    result = result.replaceAllLiterally("%H", "HH")
    result = result.replaceAllLiterally("%M", "mm")
    result = result.replaceAllLiterally("%S", "ss")
    result
  }
}
