package org.raisercostin.own

import org.raisercostin.tags.Formats
import org.scalatest._
import org.junit.runner.RunWith
import org.raisercostin.tags.FormatInterpolator
import org.joda.time.LocalDateTime
import org.joda.time.DateTimeZone
import org.joda.time.DateTime
import org.raisercostin.tags.FormatAnalyser
import org.raisercostin.tags.FormatAnalyser

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class FormatInterpolatorTest extends FunSuite with BeforeAndAfterAll with TryValues {
  import org.junit.Assert._
  import org.scalatest.Matchers._
  test("test interpolator with invalid format") {
    val int = FormatInterpolator(Map("a" -> new org.joda.time.DateTime(0).toString(Formats.dateTimeInternalExifFormatter)))
    import org.scalatest.Matchers._
    //int("$a(an'an':%Y-'luna':%m-'zi':%d 'ora':%H-'min':%M-'sec':%S)").failure.exception.printStackTrace()
    int("$a(an'an':%Y-'luna':%m-'zi':%d 'ora':%H-'min':%M-'sec':%S)").failure.exception.getMessage() should include("Can't interpolate")
  }
  test("test interpolator") {
    val tags = Map(
      "key1" -> new org.joda.time.DateTime(2015, 1, 6, 11, 44, 8, 0, DateTimeZone.UTC).toString(Formats.dateTimeInternalExifFormatter),
      "exifFileNumberMajor" -> "437",
      "exifFileNumberMinor" -> "2366",
      "fileExtension" -> "THM")
    val int = FormatInterpolator(tags)
    assertEquals("2015:01:06 11:44:08+0000", tags("key1"))

    assertEquals("2015-01-06--11-44-08", int("$key1(%Y-%m-%d--%H-%M-%S)").get)
    assertEquals("default", int("$test1|$test2|default").get)
    assertEquals("prefixsuffix", int("$test1|$test2|(prefix%%suffix)").get)
    assertEquals("", int("$test1|$test2|(prefix%%suffix|)").get)
    assertEquals("prefixdefaultsuffix", int("$test1|$test2|default(prefix%%suffix)").get)
    assertEquals("prefix2015:01:06 11:44:08+0000suffix - an:2015-luna:01-zi:06 ora:11-min:44-sec:08", int("$test1|$test2|$key1|(prefix%%suffix) - $key1('an':%Y-'luna':%m-'zi':%d 'ora':%H-'min':%M-'sec':%S)").get)
    assertEquals("2015-01-06--11-44-08---437-IMG_2366.THM", int("$exifE36867|$exifModifyDate|$key1|(%Y-%m-%d--%H-%M-%S|XXXX-XX-XX--XX-XX-XX)---$exifFileNumberMajor|(%%)-IMG_$exifFileNumberMinor(%%)$compClosestLocation|(--%%|)$compRemaining|(--%%|)$fileExtension(.%%)").get)
    assertEquals("XXXX-XX-XX--XX-XX-XX---437-IMG_2366.THM", int("$exifE36867|$exifModifyDate2|$key12|(%Y-%m-%d--%H-%M-%S|XXXX-XX-XX--XX-XX-XX)---$exifFileNumberMajor|(%%)-IMG_$exifFileNumberMinor(%%)$compClosestLocation|(--%%|)$compRemaining|(--%%|)$fileExtension(.%%)").get)
  }
  test("test interpolator with key with special characters") {
    val int = FormatInterpolator(Map("key1#test" -> "value1"))
    assertEquals("value1", int("$key1#test").get)
  }
  test("Special timezone as unknown timezone marker") {
    val local = new DateTime(2015, 1, 28, 9, 29, 0, 0, DateTimeZone.forOffsetMillis(Int.MaxValue))
    assertEquals("2015-01-28T09:29:00.000+596:31:23.647", local.toString())
  }
  test("can't construct a DateTime from a time with missing timezone") {
    val local = Formats.toStandard(new LocalDateTime(2015, 1, 28, 9, 29, 0, 0))
    assertEquals("2015-01-28T09:29:00", local)
    Formats.extractDateTime(local).toString() should startWith("Failure(java.lang.RuntimeException: Couldn't extract a DateTime from value [2015-01-28T09:29:00] using formatters:")
    assertEquals("2015-01-28T09:29:00.000", Formats.extractLocalDateTime(local).get.toString())
  }
  test("format interpolator with multiple formatters") {
    val localDateTime = new LocalDateTime(2015, 1, 28, 9, 29, 0, 0)
    val dateTime = localDateTime.toDateTime(DateTimeZone.forOffsetHours(13))
    val int = FormatInterpolator(Map("date1" -> Formats.toStandard(localDateTime), "date2" -> Formats.toStandard(dateTime)))
    assertEquals("2015-01-28--09-29-00 timezone: unknown", int("$date1(%Y-%m-%d--%H-%M-%S 'timezone: unknown')|$date2(%Y-%m-%d--%H-%M-%S 'timezone: 'Z)").get)
  }
  test("test formatting times without time zone") {
    val localDateTime = new LocalDateTime(2015, 1, 28, 9, 29, 0, 0)
    val dateTime = localDateTime.toDateTime(DateTimeZone.forOffsetHours(13))
    val int = FormatInterpolator(Map("date1" -> Formats.toStandard(localDateTime), "date2" -> Formats.toStandard(dateTime)))
    assertEquals("2015:01:28 09:29:00", localDateTime.toString(Formats.dateTimeInternalExifFormatter))
    assertEquals("2015-01-28T09:29:00", int("$date1").get)
    assertEquals("2015-01-28--09-29-00 timezone: unknown", int("$date1(%Y-%m-%d--%H-%M-%S 'timezone: unknown')").get)
    assertEquals("2015:01:28 09:29:00+1300", dateTime.toString(Formats.dateTimeInternalExifFormatter))
    assertEquals("2015-01-28T09:29:00.000+13:00", int("$date2").get)
    assertEquals("2015-01-28--09-29-00 timezone: +1300", int("$date2(%Y-%m-%d--%H-%M-%S 'timezone: 'Z)").get)
    assertEquals("date1=2015-01-28--09-29-00 timezone: unknown date2=2015-01-28--09-29-00 timezone: +1300", int("date1=$date1(%Y-%m-%d--%H-%M-%S 'timezone: unknown') date2=$date2(%Y-%m-%d--%H-%M-%S 'timezone: 'Z)").get)
    assertEquals("2015-01-28--09-29-00 timezone: unknown", int("$date1(%Y-%m-%d--%H-%M-%S 'timezone: unknown')|$date2(%Y-%m-%d--%H-%M-%S 'timezone: 'Z)").get)
    assertEquals("2015-01-28--09-29-00 timezone: +1300", int("$date2(%Y-%m-%d--%H-%M-%S 'timezone: 'Z)|$date2(%Y-%m-%d--%H-%M-%S 'timezone: unknown')").get)
  }
  test("test find variable") {
    val tags = FormatInterpolator(1.to(10).map(i => "key" + i -> ("value" + i)).toMap)
    assertEquals(None, tags("$keyA").toOption)
    assertEquals("value1", tags("$key1").get)
    assertEquals("value1_", tags("$key1_").get)
    assertEquals("value1", tags("$keyA|$key1").get)
    assertEquals("value1", tags("$keyA|$keyB|$keyC|$key1").get)
    assertEquals("value2", tags("$keyA|value2").get)
    assertEquals("value2", tags("$keyA|$keyB|value2").get)
    assertEquals("value2", tags("$keyA|$keyB|$keyC|value2").get)
    assertEquals("value2|value3", tags("$keyA|value2|value3").get)
    assertEquals("", tags("$keyA|").get)
    assertEquals("prefixsuffix", tags("$keyA|(prefix%%suffix)").get)
    assertEquals("", tags("$a|").get)
    assertEquals("", tags("$a|(%%)").get)
    assertEquals("()", tags("$a|()").get)
    assertEquals("prefix[default]suffix-noA", tags("""$keyA(prefix%%suffix)|$keyB(prefix%%suffix)|$keyC|$keyD|$keyE|default(prefix[%%]suffix)$a|(%%|-noA)""").get)
    assertEquals("goodPrefix[value1]goodSuffix", tags("$keyA(prefix%%suffix)|$key1(goodPrefix[%%]goodSuffix)").get)
    assertEquals("format2", tags("$keyA(format)|(format2)").get)
    assertEquals("format3", tags("$keyA(format)|$keyB(format2)|(format3)").get)
  }
  test("test dateAnalyser") {
    val tags = FormatInterpolator(Map())
    assertEquals("XXXX-XX-XX--XX-XX-XX+XXXX", tags(FormatAnalyser.dateAnalyser).get)
  }
}

