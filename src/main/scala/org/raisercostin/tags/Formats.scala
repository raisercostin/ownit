package org.raisercostin.tags

import org.joda.time.LocalDateTime
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
      simpleConverter.orElse(timeConverter)((format, value))

  def simpleConverter: PartialFunction[(String, String), Try[String]] = {
    case (format, value) if format.contains("%%") => Success(format.replaceAllLiterally("%%", value))
  }
  def timeConverter: PartialFunction[(String, String), Try[String]] = {
    case (format, value) =>
      //println(s"convert[$format][$value]")
      //assume is date
      extractDate(value).map { date =>
        val format2 = fromIrfanViewToJodaDateTime(format)
        date.toString(format2.replaceAll("%", ""))
      } match {
        case Success(s) => Success(s)
        case Failure(ex) => Failure(new RuntimeException(s"Couldn't format date with [$format] " + ex.getMessage(), ex))
      }
  }

  val exifDateTimeFormatterPattern = "yyyy:MM:dd HH:mm:ss"
  val exifDateTimeFormatter = org.joda.time.format.DateTimeFormat.forPattern(exifDateTimeFormatterPattern)
  private val exifDateTimeFormatter2 = org.joda.time.format.DateTimeFormat.forPattern("yyyy:MM:dd HH:mm:ssZ").withOffsetParsed()
  private val exifDateTimeFormatter3 = org.joda.time.format.DateTimeFormat.forPattern("yyyy:00:00 00:00:00")
  def extractDate(value: Any): Try[org.joda.time.DateTime] = {
    import org.joda.time.DateTime
    import org.joda.time.format.DateTimeFormat
    import java.util.GregorianCalendar
    import java.util.Date
    value match {
      case text: String =>
        Try { DateTime.parse(text.trim, exifDateTimeFormatter) }.
          orElse(Try { DateTime.parse(text.trim, exifDateTimeFormatter2) }).
          orElse(Try { DateTime.parse(text.trim, exifDateTimeFormatter3) })
      case date: Date =>
        Try { new DateTime(date) }
      case date: GregorianCalendar =>
        Try { new DateTime(date) }
      case x =>
        throw new IllegalArgumentException(s"Can't convert [$value] of type [${value.getClass}] to DateTime.")
    }
  }
  def extractLocalDateTime(value:String): Try[LocalDateTime] = Try{LocalDateTime.parse(value,exifDateTimeFormatter.withOffsetParsed())}
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
