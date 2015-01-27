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
import org.raisercostin.tags.Tags
import org.raisercostin.tags.FormatAnalyser

@RunWith(classOf[JUnitRunner])
class RawTest extends FunSuite with BeforeAndAfterAll with TryValues {
  test("extract exif from one file") {
    val tags = raw.all(false)(Locations.classpath("MVI_2366.MOV"))
    assertEquals(67, tags.size)
  }
  test("extract exif from one pair too") {
    val tags = raw.all(true)(Locations.classpath("MVI_2366.MOV"))
    assertEquals(234, tags.size)
  }
  test("extractor that combines MOV and THM", Tag("failed"), Tag("feature")) {
    val file = Locations.classpath("MVI_2366.MOV")
    val tags = raw.loadExifTags(file)
    val all = tags.tags.tags.toSeq.sortBy(_._1)
    println(all.mkString("\n"))
    assertEquals("MOV", tags.fileExtension.get)
    assertEquals("${const:MVI}_${exifFileNumberMinor}.${fileExtension}", tags.analyse(file.name).get)
    assertEquals(239, all.size)
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

  def extract(file: String, discoverPairs: Boolean = true): String = Tags(raw.all(discoverPairs)(Locations.classpath(file))).interpolate("$exifCreateDate#THM|$exifCreateDate").get

  //on G11
  //the dailight saving time modifies the creation date (you can detect this only by comparison with other files
  //mov use the utc time
  //jpg and thm (associated with mov) use the local time
  test("times1") {
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), extract("time1-IMG_2384.JPG"))
  }
  ignore("times1-sanselan") {
    checkTime(new DateTime(2015, 1, 9, 0, 0, 36, local), raw.extractor.sanselanExifExtractor(Locations.classpath("time1-IMG_2384.JPG"))("time1-IMG_2384.JPG"))
  }
  ignore("times1-fileAttribute") {
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
  ignore("find similar to movie image file") {
    val src = Locations.classpath("time6-MVI_2389.MOV")
    val tags = raw.loadExifTags(src)
    val fileNameFormat = tags.analyse(src.name).get
    assertEquals("time6-${const:MVI}_${exifFileNumberMinor}.${fileExtension}", fileNameFormat)
    assertEquals("time6",FormatAnalyser.cleanFormat(fileNameFormat))
    val variable = FormatAnalyser.cleanFormat(fileNameFormat)
    val fileNamePattern =fileNameFormat.replaceAllLiterally(variable,"") 
    assertEquals("-${const:MVI}_${exifFileNumberMinor}.${fileExtension}",fileNamePattern)
    val numberMinor = tags.fileNumberMinor.get
    val fileNumber = tags.fileNumber.get
    val delta = 10
    val range = numberMinor - delta to numberMinor + delta
      //search for a jpg that has a counter slightly before the current exifFileNumber
      def nameMightContainNumberInRange(range: Range)(name: String) = range.find(x => name contains x.toString).isDefined
      
    val files = src.asFile.parent.list.filter(_.extension.toLowerCase == "jpg").toList
    //(1 to delta).find(x )
    val pairs = files.//filter { file => nameMightContainNumberInRange(range)(file.name) }.
    	map{x=>println(s"fileNumber in $x");(x,raw.loadExifTags(x).fileNumber.map{_-fileNumber})}.
    	filter(x=>x._2.getOrElse(0)!=0).
    	map(x=>(x._1,x._2.get.abs)).
    	toList.sortBy(_._2)
    assertEquals("", pairs.mkString("\n"))
  }
}