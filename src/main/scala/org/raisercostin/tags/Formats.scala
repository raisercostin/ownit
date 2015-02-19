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
import org.joda.time.tz.DateTimeZoneBuilder
import org.raisercostin.exif.ExifTags

trait Formatter {
  def convert(value: String, convertor: Option[String] = None, convertorNull: Option[String] = None): Try[String]
}
trait Validator {
  def validateDateTime(dateTime: DateTime): Option[DateTime]
  def validateLocalDateTime(dateTime: LocalDateTime): Option[LocalDateTime]
}
object Formats extends Formats(None) {
  val logger = org.slf4j.LoggerFactory.getLogger(Formats.getClass())
}
case class Formats(val validator: Option[Validator]) extends Formatter {
  import Formats._
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
    else {
      val formatters = Seq(
        simpleConverter,
        dateTimeConverter,
        localDateTimeConverter,
        fallbackDateTimeConverter(extractDateTimeGeneric),
        fallbackDateTimeConverter(extractLocalDateTimeGeneric),
        useFormatString)
      val all = formatters.toStream.map(formatter => Try { formatter(format, value) }.getOrElse(Failure(new RuntimeException("Partial applied function " + formatter + " failed."))))
      val result = all.find(x => x.isSuccess)
      result.getOrElse(all.last)
      //((simpleConverter orElse dateTimeConverter orElse localDateTimeConverter orElse fallbackDateTimeConverter(extractDateTimeGeneric) orElse fallbackDateTimeConverter(extractLocalDateTimeGeneric))((format, value))).orElse(useFormatString(format, value))
    }

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
  def fallbackDateTimeConverter(genericExtractor: Any => Try[{ def toString(pattern: String): String }]): PartialFunction[(String, String), Try[String]] = {
    case (format, value) =>
      //println(s"convert[$format][$value]")
      //assume is date
      genericExtractor(value).map { date =>
        logger.warn(s"Fallback convertor worked for format[$format] and value[$value] by detecting date[$date]")
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
    case (format, value) =>
      Failure(new RuntimeException(s"Couldn't format value [$value] using format [$format]"))
  }
  object Named {
    implicit def toT[T](named: Named[T]): T = named.value
    def apply[T](name: String, generator: (String) => T) = new Named[T](name, generator(name))
  }
  case class Named[T](name: String, value: T)

  val localDateTimeInternalExifFormatter = Named("yyyy:MM:dd HH:mm:ss", DateTimeFormat.forPattern _)
  val localDateTimeInternalExifFormatterPattern = localDateTimeInternalExifFormatter.name
  val localDateTimeFormatterISO = Named("yyyy-MM-dd'T'HH:mm:ss", DateTimeFormat.forPattern _)
  def toStandard(value: LocalDateTime): String = value.toString(localDateTimeFormatterISO)

  private val patternWithOffsetParsed = (x: String) => DateTimeFormat.forPattern(x).withOffsetParsed()
  private val patternWithoutOffsetParsed = (x: String) => DateTimeFormat.forPattern(x)
  val dateTime4 = Named("yyyy-MM-dd'T'HH:mm:ssZ", patternWithOffsetParsed)
  val dateTimePattern = dateTime4.name
  val dateTimeInternalExifFormatter = Named("yyyy:MM:dd HH:mm:ssZ", patternWithOffsetParsed)
  val dateTimeInternalExifFormatter2 = Named("yyyy:MM:dd HH:mm:ss.SSSZ", patternWithOffsetParsed)
  private val exifDateTimeFormatter3 = Named("yyyy:00:00 00:00:00", patternWithoutOffsetParsed)
  private val dateTimeIsoFormatter = Named("yyyy-MM-dd'T'HH:mm:ss.SSSZZ", ISODateTimeFormat.dateTime().withOffsetParsed())
  def toStandard(value: DateTime): String = value.toString(dateTimeIsoFormatter)
  def toStandard(value: DateTimeZone): String = value.toString
  def toSimplified(value: DateTimeZone): String = dateTimeZoneFormatter.print(new DateTime(0, value))

  private val dateTimeZoneFormatter = Named("Z", patternWithOffsetParsed)
  def extractDateTimeZone(value: String): Try[DateTimeZone] = value match { //Try{DateTimeZone.forID(value)}
    case "UTC" =>
      Success(DateTimeZone.UTC)
    case value =>
      Try { dateTimeZoneFormatter.parseDateTime(value).getZone() }
  }
  //def extractLocalDateTime(value: String): Try[LocalDateTime] = Try { LocalDateTime.parse(value, localDateTimeInternalExifFormatter.withOffsetParsed()) }
  def extractLocalDateTimeGeneric(value: Any): Try[org.joda.time.LocalDateTime] = {
    val text = value.toString.replaceAll("""[ \W]+""", "")
    parseLocalDateTime("yyyyMMddHHmmss", text)
  }

  def parseLocalDateTime(format: String, value: String): Try[LocalDateTime] = {
    val formatter = Named(format, patternWithoutOffsetParsed)
    Try { validateLocalDateTime(LocalDateTime.parse(value.trim, formatter)) }
  }

  def extractDateTimeGeneric(value: Any): Try[org.joda.time.DateTime] = {
    val text = value.toString.replaceAll("""[ \W]+""", "")
    val formatter = Named("yyyyMMddHHmmssZ", patternWithOffsetParsed)
    Try { validateDateTime(DateTime.parse(text.trim, formatter)) }
  }

  def extractLocalDateTime(value: Any): Try[LocalDateTime] = {
    import org.joda.time.DateTime
    import org.joda.time.format.DateTimeFormat
    import java.util.GregorianCalendar
    import java.util.Date
    val result = value match {
      case text: String =>
        val formatters = Seq(localDateTimeInternalExifFormatter, localDateTimeFormatterISO, Named("yyyy-MM-dd HH:mm:ss", patternWithoutOffsetParsed))
          def parse(format: Named[DateTimeFormatter]) = Try { LocalDateTime.parse(text.trim, format) }
        parseInOrder("LocalDateTime", formatters, parse, text.trim)
      case date: Date =>
        Try { new LocalDateTime(date) }
      case date: GregorianCalendar =>
        Try { new LocalDateTime(date) }
      case x =>
        throw new IllegalArgumentException(s"Can't convert [$value] of type [${value.getClass}] to LocalDateTime.")
    }
    result.map(validateLocalDateTime)
  }
  def extractDateTime(value: Any): Try[org.joda.time.DateTime] = {
    import org.joda.time.DateTime
    import org.joda.time.format.DateTimeFormat
    import java.util.GregorianCalendar
    import java.util.Date
    val result = value match {
      case text: String =>
        val formatters = Seq(dateTime4, Named("yyyy-MM-dd HH:mm:ssZ", patternWithOffsetParsed), dateTimeIsoFormatter, dateTimeInternalExifFormatter2, dateTimeInternalExifFormatter, exifDateTimeFormatter3)
          def parseDateTime(format: Named[DateTimeFormatter]) = Try { DateTime.parse(text.trim, format) }
        parseInOrder("DateTime", formatters, parseDateTime, text.trim)
      case date: Date =>
        Try { new DateTime(date) }
      case date: GregorianCalendar =>
        Try { new DateTime(date) }
      case x =>
        throw new IllegalArgumentException(s"Can't convert [$value] of type [${value.getClass}] to DateTime.")
    }
    result.map(validateDateTime)
  }

  def validateDateTime(value: DateTime): DateTime =
    validator match {
      case Some(v) =>
        v.validateDateTime(value).getOrElse(
          throw new RuntimeException(s"Invalid date [$value]."))
      case None =>
        value
    }

  def validateLocalDateTime(value: LocalDateTime): LocalDateTime =
    validator match {
      case Some(v) =>
        v.validateLocalDateTime(value).getOrElse(
          throw new RuntimeException(s"Invalid date [$value]."))
      case None =>
        value
    }

  private def failureDetail[U](detail: String, value: String, formatters: Seq[Any]): Throwable => Try[U] = f => Failure[U](new RuntimeException(s"Couldn't extract a ${detail} from value [$value] using formatters:\n\t${formatters.mkString("\n\t")}.", f))
  def parseInOrder[T, U](detail: String, formatters: Seq[T], parse: T => Try[U], value: String): Try[U] =
    formatters.drop(1).foldLeft(parse(formatters.head))((prev, formatter) => prev.orElse(parse(formatter)))
      .
      transform(
        x => Success(x),
        failureDetail(detail, value, formatters))

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
