package org.raisercostin.own

import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner
import org.raisercostin.util.io.Locations
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.tz.DateTimeZoneBuilder

@RunWith(classOf[JUnitRunner])
class RawTest extends FunSuite with BeforeAndAfterAll {
  import org.raisercostin.exif._
  import org.raisercostin.exif.RichExif._
  test("extract exif from one file") {
    val tags = ExternalExifExtractor(false).extract(Locations.classpath("MVI_2366.MOV")).map(_.tags).getOrElse(Map())
    assertEquals(69, tags.size)
  }
  test("extract exif from one pair too") {
    val tags = ExternalExifExtractor(true).extract(Locations.classpath("MVI_2366.MOV")).map(_.tags).getOrElse(Map())
    assertEquals(214, tags.size)
  }
  test("best exif extractor") {
    val tags = BestExifExtractor.extract(Locations.classpath("MVI_2366.MOV")).map(_.tags).getOrElse(Map())
    println(tags.mkString("\n"))
    assertEquals(214, tags.size)
  }
  def checkTime(expectedDateTime: DateTime, exifValue: String) =
    assertEquals("Picture taken at [" + expectedDateTime + "] with timezone " + expectedDateTime.getZone() + " at " + expectedDateTime.toDateTimeISO(),
      expectedDateTime.toString("yyyy:MM:dd HH:mm:ss"), exifValue)
  val local = DateTimeZone.forOffsetHours(2)
  
  def extract(file:String,discoverPairs:Boolean = true):String = extract(file,ExternalExifExtractor(discoverPairs))
  def extract(file:String,extractor:Extractor):String = extractor.extract(Locations.classpath(file)).map(_.tags("exifCreateDate")).getOrElse("")  
  //on G11
  //the dailight saving time modifies the creation date (you can detect this only by comparison with other files
  //mov use the utc time
  //jpg and thm (associated with mov) use the local time
  test("times1") {
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), extract("time1-IMG_2384.JPG"))
  }
  test("times1-sanselan") {
    println(SanselanExifExtractor.extract(Locations.classpath("time1-IMG_2384.JPG"))map(_.tags.mkString("\n")))
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), extract("time1-IMG_2384.JPG",SanselanExifExtractor))
  }
  test("times1-fileAttribute") {
    println(FileAttributesExtractor.extract(Locations.classpath("time1-IMG_2384.JPG"))map(_.tags.mkString("\n")))
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), extract("time1-IMG_2384.JPG",FileAttributesExtractor))
  }
  test("times2") {
    checkTime(new DateTime(2015, 1, 9, 0, 0, 42, local), extract("time2-MVI_2385.THM",false))
    checkTime(new DateTime(2015, 1, 9, 0, 0, 42, local).withZone(DateTimeZone.UTC), extract("time2-MVI_2385.MOV",false))
    checkTime(new DateTime(2015, 1, 9, 0, 0, 42, local), extract("time2-MVI_2385.MOV"))
  }
  test("times3 in utc+2 and dailight saving time") {
    println(SanselanExifExtractor.extract(Locations.classpath("time3-IMG_2386.JPG"))map(_.tags.mkString("\n")))
    checkTime(new DateTime(2015, 1, 9, 0, 1, 31, local), extract("time3-IMG_2386.JPG",SanselanExifExtractor))
    checkTime(new DateTime(2015, 1, 9, 0, 1, 31, local), extract("time3-IMG_2386.JPG"))
  }
  test("times4 in utc+2 and dailight saving time") {
    checkTime(new DateTime(2015, 1, 9, 0, 1, 39, local), extract("time4-MVI_2387.THM",false))
    checkTime(new DateTime(2015, 1, 9, 0, 1, 39, local).withZone(DateTimeZone.UTC), extract("time4-MVI_2387.MOV",false))
    checkTime(new DateTime(2015, 1, 9, 0, 1, 39, local), extract("time4-MVI_2387.MOV"))
  }
  test("times5") {
    checkTime(new DateTime(2015, 1, 9, 0, 2, 36, local), extract("time5-IMG_2388.JPG"))
  }
  test("times6") {
    checkTime(new DateTime(2015, 1, 9, 0, 2, 46, local), extract("time6-MVI_2389.THM",false))
    checkTime(new DateTime(2015, 1, 9, 0, 2, 46, local).withZone(DateTimeZone.UTC), extract("time6-MVI_2389.MOV",false))
    checkTime(new DateTime(2015, 1, 9, 0, 2, 46, local), extract("time6-MVI_2389.MOV"))
  }
}