package org.raisercostin.own

import org.raisercostin.tags.Formats;
import org.raisercostin.tags.raw;
import org.scalatest._
import org.junit.runner.RunWith

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class FormatInterpolatorTest extends FunSuite with BeforeAndAfterAll with TryValues {
  import org.junit.Assert._
  test("test interpolator with invalid format") {
    val int = raw.interpolator(Map("a" -> new org.joda.time.DateTime(0).toString(Formats.exifDateTimeFormatter)))
import org.scalatest.Matchers._
    int("$a(an'an':%Y-'luna':%m-'zi':%d 'ora':%H-'min':%M-'sec':%S)").failure.exception.getMessage() should include("Couldn't format date")
  }
  test("test interpolator") {
    val tags = Map(
      "key1" -> new org.joda.time.DateTime(2015, 1, 6, 11, 44, 8, 0).toString(Formats.exifDateTimeFormatter),
      "exifFileNumberMajor" -> "437",
      "exifFileNumberMinor" -> "2366",
      "fileExtension" -> "THM")
    val int = raw.interpolator(tags)
    assertEquals("2015:01:06 11:44:08", tags("key1"))

    assertEquals("2015-01-06--11-44-08", int("$key1(%Y-%m-%d--%H-%M-%S)").get)
    assertEquals("default", int("$test1|$test2|default").get)
    assertEquals("prefixsuffix", int("$test1|$test2|(prefix%%suffix)").get)
    assertEquals("", int("$test1|$test2|(prefix%%suffix|)").get)
    assertEquals("prefixdefaultsuffix", int("$test1|$test2|default(prefix%%suffix)").get)
    assertEquals("prefix2015:01:06 11:44:08suffix - an:2015-luna:01-zi:06 ora:11-min:44-sec:08", int("$test1|$test2|$key1|(prefix%%suffix) - $key1('an':%Y-'luna':%m-'zi':%d 'ora':%H-'min':%M-'sec':%S)").get)
    assertEquals("2015-01-06--11-44-08---437-IMG_2366.THM", int("$exifE36867|$exifModifyDate|$key1|(%Y-%m-%d--%H-%M-%S|XXXX-XX-XX--XX-XX-XX)---$exifFileNumberMajor|(%%)-IMG_$exifFileNumberMinor(%%)$compClosestLocation|(--%%|)$compRemaining|(--%%|)$fileExtension(.%%)").get)
    assertEquals("XXXX-XX-XX--XX-XX-XX---437-IMG_2366.THM", int("$exifE36867|$exifModifyDate2|$key12|(%Y-%m-%d--%H-%M-%S|XXXX-XX-XX--XX-XX-XX)---$exifFileNumberMajor|(%%)-IMG_$exifFileNumberMinor(%%)$compClosestLocation|(--%%|)$compRemaining|(--%%|)$fileExtension(.%%)").get)
  }
}
