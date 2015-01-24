package org.raisercostin.own

import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner
import org.raisercostin.tags.raw
import org.raisercostin.util.io.Locations
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.tz.DateTimeZoneBuilder
import org.raisercostin.util.io.InputLocation
import org.raisercostin.tags.Item

@RunWith(classOf[JUnitRunner])
class RawTest extends FunSuite with BeforeAndAfterAll with TryValues {
  test("extract exif from one file") {
    val tags = raw.all(false)(Locations.classpath("MVI_2366.MOV"))
    assertEquals(67, tags.size)
  }
  test("extract exif from one pair too") {
    val tags = raw.all(true)(Locations.classpath("MVI_2366.MOV"))
    assertEquals(212, tags.size)
  }
  test("extractor that combines MOV and THM", Tag("failed"), Tag("feature")) {
    val tags = raw.loadExifTags(Locations.classpath("MVI_2366.MOV")).tags.tags.toSeq.sortBy(_._1)
    assertEquals(215, tags.size)
    //val tags2 = raw2.BestExifExtractor.extract(Locations.classpath("MVI_2366.MOV")).get.tags.toSeq.sortBy(_._1)
    //println(tags.mkString("\n"))
    //    assertEquals(214, tags2.size)
    //    assertEquals(214, tags.size)
    //assertEquals(tags2.mkString("\n"), tags.mkString("\n"))
  }
  def checkTime(expectedDateTime: DateTime, exifValue: String) =
    assertEquals("Picture taken at [" + expectedDateTime + "] with timezone " + expectedDateTime.getZone() + " at " + expectedDateTime.toDateTimeISO(),
      expectedDateTime.toString("yyyy:MM:dd HH:mm:ss"), exifValue)
  val local = DateTimeZone.forOffsetHours(2)

  def extract(file: String, discoverPairs: Boolean = true): String = raw.all(discoverPairs)(Locations.classpath(file)).get("exifCreateDate").get
  //on G11
  //the dailight saving time modifies the creation date (you can detect this only by comparison with other files
  //mov use the utc time
  //jpg and thm (associated with mov) use the local time
  test("times1") {
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), extract("time1-IMG_2384.JPG"))
  }
  ignore("times1-sanselan") {
    println(raw.extractor.sanselanExifExtractor(Locations.classpath("time1-IMG_2384.JPG")).mkString("\n"))
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), raw.extractor.sanselanExifExtractor(Locations.classpath("time1-IMG_2384.JPG"))("time1-IMG_2384.JPG"))
  }
  ignore("times1-fileAttribute") {
    println(raw.extractor.fileAttributesExtractor(Locations.classpath("time1-IMG_2384.JPG")).mkString("\n"))
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), raw.extractor.fileAttributesExtractor(Locations.classpath("time1-IMG_2384.JPG"))("time1-IMG_2384.JPG"))
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