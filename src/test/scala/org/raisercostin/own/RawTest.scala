package org.raisercostin.own

import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner
import org.raisercostin.util.io.Locations
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.tz.DateTimeZoneBuilder
import org.raisercostin.util.io.InputLocation

@RunWith(classOf[JUnitRunner])
class InterpolationTest extends FunSuite with BeforeAndAfterAll with TryValues {
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

@RunWith(classOf[JUnitRunner])
class AnalyserTest extends FunSuite with BeforeAndAfterAll with TryValues {
  val tags2 = Map(
    "exifDateTimeOriginal" -> new org.joda.time.DateTime(2015, 1, 6, 11, 44, 8, 0).toString(Formats.exifDateTimeFormatter),
    "exifFileNumberMajor" -> "437",
    "exifFileNumberMinor" -> "2366",
    "fileExtension" -> "THM")
  val analyse = raw.analyser(tags2)

  test("analyse key with const") {
    assertEquals("${const:IMG}_${exifFileNumberMinor}",analyse("IMG_2366").get)
  }
  test("analyse with formatter",Tag("failed")) {
    assertEquals("$exifFileNumberMinor(IMG_%%)",analyse("IMG_2366").get)
  }
  test("test analyser") {
    assertEquals("${exifFileNumberMajor}", analyse("437").get)
    assertEquals("${exifFileNumberMinor}", analyse("2366").get)
    assertEquals("${exifFileNumberMajor}/${const:IMG}_${exifFileNumberMinor}.thm", analyse("437/IMG_2366.thm").get)
  }
  test("analyze date format") {
    assertEquals("${exifDateTimeOriginal+yyyy}${exifDateTimeOriginal+MM}${exifDateTimeOriginal+dd}_${exifDateTimeOriginal+HH}${exifDateTimeOriginal+mm}${exifDateTimeOriginal+ss}.jpg", analyse("20150106_114408.jpg").get)
    assertEquals("${exifDateTimeOriginal+1s+yyyy}${exifDateTimeOriginal+1s+MM}${exifDateTimeOriginal+1s+dd}_${exifDateTimeOriginal+1s+HH}${exifDateTimeOriginal+1s+mm}${exifDateTimeOriginal+1s+ss}.jpg", analyse("20150106_114409.jpg").get)
    assertEquals("${exifDateTimeOriginal+2s+yyyy}${exifDateTimeOriginal+2s+MM}${exifDateTimeOriginal+2s+dd}_${exifDateTimeOriginal+2s+HH}${exifDateTimeOriginal+2s+mm}${exifDateTimeOriginal+2s+ss}.jpg", analyse("20150106_114410.jpg").get)
    assertEquals("${exifDateTimeOriginal+3s+yyyy}${exifDateTimeOriginal+3s+MM}${exifDateTimeOriginal+3s+dd}_${exifDateTimeOriginal+3s+HH}${exifDateTimeOriginal+3s+mm}${exifDateTimeOriginal+3s+ss}.jpg", analyse("20150106_114411.jpg").get)
    assertEquals("20150106_114412.jpg", analyse("20150106_114412.jpg").get)
  }
  test("analyze consolidated date format",Tag("failed")) {
    assertEquals("$exifDateTimeOriginal(yyyyMMdd_HHmmss).jpg", analyse("20150106_114408.jpg").get)
  }
  test("analyze format2") {
    assertEquals("aaaa${exifFileNumberMajor}${exifFileNumberMinor}bbb", analyse("aaaa4372366bbb").get)
  }
}

@RunWith(classOf[JUnitRunner])
class RawTest extends FunSuite with BeforeAndAfterAll with TryValues {
  import org.raisercostin.exif._
  import org.raisercostin.exif.RichExif._
  test("extract exif from one file") {
    val tags = raw.externalExifExtractor(false)(Locations.classpath("MVI_2366.MOV")).tags
    assertEquals(69, tags.size)
  }
  test("extract exif from one pair too") {
    val tags = raw.externalExifExtractor(true)(Locations.classpath("MVI_2366.MOV")).tags
    assertEquals(214, tags.size)
  }
  test("best exif extractor") {
    val tags = raw.bestExifFullExtractor(true)(Locations.classpath("MVI_2366.MOV")).tags.toSeq.sortBy(_._1)
    val tags2 = raw2.BestExifExtractor.extract(Locations.classpath("MVI_2366.MOV")).get.tags.toSeq.sortBy(_._1)
    //println(tags.mkString("\n"))
    //    assertEquals(214, tags2.size)
    //    assertEquals(214, tags.size)
    assertEquals(tags2.mkString("\n"), tags.mkString("\n"))
  }
  def checkTime(expectedDateTime: DateTime, exifValue: String) =
    assertEquals("Picture taken at [" + expectedDateTime + "] with timezone " + expectedDateTime.getZone() + " at " + expectedDateTime.toDateTimeISO(),
      expectedDateTime.toString("yyyy:MM:dd HH:mm:ss"), exifValue)
  val local = DateTimeZone.forOffsetHours(2)

  def extract(file: String, discoverPairs: Boolean = true): String = extract(file, raw.externalExifExtractor(discoverPairs))
  def extract(file: String, extractor: InputLocation => Item): String = extractor(Locations.classpath(file)).tags("exifCreateDate")
  //on G11
  //the dailight saving time modifies the creation date (you can detect this only by comparison with other files
  //mov use the utc time
  //jpg and thm (associated with mov) use the local time
  test("times1") {
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), extract("time1-IMG_2384.JPG"))
  }
  ignore("times1-sanselan") {
    println(raw.sanselanExifExtractor(Locations.classpath("time1-IMG_2384.JPG")).mkString("\n"))
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), raw.sanselanExifExtractor(Locations.classpath("time1-IMG_2384.JPG"))("time1-IMG_2384.JPG"))
  }
  ignore("times1-fileAttribute") {
    println(raw.fileAttributesExtractor(Locations.classpath("time1-IMG_2384.JPG")).mkString("\n"))
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), raw.fileAttributesExtractor(Locations.classpath("time1-IMG_2384.JPG"))("time1-IMG_2384.JPG"))
  }
  test("times2") {
    checkTime(new DateTime(2015, 1, 9, 0, 0, 42, local), extract("time2-MVI_2385.THM", false))
    checkTime(new DateTime(2015, 1, 9, 0, 0, 42, local).withZone(DateTimeZone.UTC), extract("time2-MVI_2385.MOV", false))
    checkTime(new DateTime(2015, 1, 9, 0, 0, 42, local), extract("time2-MVI_2385.MOV"))
  }
  test("times3 in utc+2 and dailight saving time") {
    //println(SanselanExifExtractor.extract(Locations.classpath("time3-IMG_2386.JPG"))map(_.tags.mkString("\n")))
    //checkTime(new DateTime(2015, 1, 9, 0, 1, 31, local), extract("time3-IMG_2386.JPG",SanselanExifExtractor))
    //the time is affected by daylight saving
    val hourWithoutDailightSavingCorrection = 1
    val hourWithDailightSavingCorrection = 0
    val expectedHour = hourWithoutDailightSavingCorrection //should be hourWithDailightSavingCorrection
    checkTime(new DateTime(2015, 1, 9, 1, hourWithoutDailightSavingCorrection, 31, local), extract("time3-IMG_2386.JPG"))
  }
  test("times4 in utc+2 and dailight saving time") {
    val hourWithoutDailightSavingCorrection = 1
    val hourWithDailightSavingCorrection = 0
    val expectedHour = hourWithoutDailightSavingCorrection //should be hourWithDailightSavingCorrection
    checkTime(new DateTime(2015, 1, 9, hourWithoutDailightSavingCorrection, 1, 39, local), extract("time4-MVI_2387.THM", false))
    checkTime(new DateTime(2015, 1, 9, hourWithDailightSavingCorrection, 1, 39, local).withZone(DateTimeZone.UTC), extract("time4-MVI_2387.MOV", false))
    checkTime(new DateTime(2015, 1, 9, hourWithoutDailightSavingCorrection, 1, 39, local), extract("time4-MVI_2387.MOV"))
  }
  test("times5") {
    checkTime(new DateTime(2015, 1, 9, 0, 2, 36, local), extract("time5-IMG_2388.JPG"))
  }
  test("times6") {
    checkTime(new DateTime(2015, 1, 9, 0, 2, 46, local), extract("time6-MVI_2389.THM", false))
    checkTime(new DateTime(2015, 1, 9, 0, 2, 46, local).withZone(DateTimeZone.UTC), extract("time6-MVI_2389.MOV", false))
    checkTime(new DateTime(2015, 1, 9, 0, 2, 46, local), extract("time6-MVI_2389.MOV"))
  }
}