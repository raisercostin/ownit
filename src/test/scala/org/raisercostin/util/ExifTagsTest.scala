package org.raisercostin.util
import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner
import org.raisercostin.tags.raw
import org.raisercostin.util.io.Locations
import org.raisercostin.util.gps.Gps
import org.raisercostin.tags.Formats
import org.joda.time.DateTimeZone
import org.joda.time.DateTime
import org.joda.time.LocalDateTime

@RunWith(classOf[JUnitRunner])
class ExifTagsTest extends FunSuite with BeforeAndAfterAll {
  import org.raisercostin.exif._
  ignore("extract exif from one file") {
    val file = Locations.classpath("20131008_175240.jpg")
    val result = raw.extractor.sanselanExifExtractor(file)
    val result2 = raw.loadExifTags(file).tags.tags
    assertEquals(result.mkString("\n"), result2.mkString("\n"))
    assertEquals(55, result.size)
    assertEquals("", result.mkString("\n"))
  }

  test("interpolate tags") {
    val file = Locations.classpath("20131008_175240.jpg")
    val tags = raw.loadExifTags(file)
    val newName = tags.interpolate("$exifE36867|$exifModifyDate|$exifDateTimeOriginal(%Y-%m-%d--%H-%M-%S)$exifFileNumber|(---%%)$compRemaining(---%%).$fileExtension").get.replaceAll("[-]+[.]", ".")
    assertEquals("2013-10-08--17-52-39.jpg", newName)
  }

  test("bug - use first datetimeoriginal") {
    val file = Locations.classpath("DSC_0547.JPG")
    val tags = raw.loadExifTags(file)
    println(tags.tags.tags.mkString("\n"))
    val newName = tags.interpolate("$exifDateTimeOriginal(%Y-%m-%d--%H-%M-%S)$exifFileNumber|(---%%)$compRemaining(---%%).$fileExtension").get.replaceAll("[-]+[.]", ".")
    assertEquals("2011-11-19--03-00-20------DSC_0547.JPG", newName)
  }
  test("bug - invalid timezone"){
    val file = Locations.classpath("IMG_2057.JPG")
    val tags = raw.loadExifTags(file)
    println(tags.tags.tags.mkString("\n"))
    assertEquals("+02:00",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T14:04:38"),new DateTime("2013-08-09T12:04:39Z")).toString)
    assertEquals("+02:00",tags.dateTimeZone.get.toString())
    assertEquals("2013-08-09--14-04-38+0200", tags.interpolate("$dateTime(%Y-%m-%d--%H-%M-%SZ)").get)
  }
  test("compute timezone"){
    assertEquals("+02:00",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T14:04:38"),new DateTime("2013-08-09T12:04:39Z")).toString)
    assertEquals("UTC",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T17:00:00"),new DateTime("2013-08-09T17:00:00Z")).toString)
    assertEquals("UTC",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T17:14:00"),new DateTime("2013-08-09T17:00:00Z")).toString)
    assertEquals("UTC",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T17:15:00"),new DateTime("2013-08-09T17:00:00Z")).toString)
    assertEquals("+00:30",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T17:16:00"),new DateTime("2013-08-09T17:00:00Z")).toString)
    assertEquals("+00:30",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T17:30:00"),new DateTime("2013-08-09T17:00:00Z")).toString)
    assertEquals("+00:30",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T17:45:00"),new DateTime("2013-08-09T17:00:00Z")).toString)
    assertEquals("+01:00",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T17:46:00"),new DateTime("2013-08-09T17:00:00Z")).toString)
    assertEquals("+01:00",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T18:15:00"),new DateTime("2013-08-09T17:00:00Z")).toString)
    assertEquals("+01:30",ExifTags.computeTimezone(new LocalDateTime("2013-08-09T18:16:00"),new DateTime("2013-08-09T17:00:00Z")).toString)
  }
    
  test("compute remaining") {
    val file = Locations.classpath("20131008_175240.jpg")
    val tags = raw.loadExifTags(file)
    println(tags.tags.tags.mkString("\n"))
    val expected="${exifModifyDate+1s+yyyy}${exifModifyDate+1s+MM}${exifModifyDate+1s+dd}_${exifModifyDate+1s+HH}${exifModifyDate+1s+mm}${exifModifyDate+1s+ss}.${fileExtension}"
    assertEquals(expected, tags.analyse("20131008_175240.jpg").get)
    assertEquals(expected, tags.compDetectedFormat.get)
    assertEquals(expected, tags.getString("compDetectedFormat").get)
    assertEquals("", tags.getString("compRemaining").get)
  }
  test("analyze exifFileNumber") {
    val file = Locations.classpath("IMG_1558.JPG")
    val tags = raw.loadExifTags(file)
    assertEquals("${const:IMG}_${exifFileNumberMinor}.${fileExtension}", tags.analyse(file.name).get)
    assertEquals("${const:IMG}_${exifFileNumberMinor}.${fileExtension}", tags.interpolate("$compDetectedFormat").get);
    assertEquals("", tags.interpolate("$compRemaining").get);
  }
  test("exif detected format") {
    val file = Locations.classpath("IMG_1558.JPG")
    val tags: ExifTags = raw.loadExifTags(file)
    assertEquals(1558, tags.fileNumberMinor.get)
    assertEquals(401, tags.fileNumberMajor.get)
    assertEquals(4011558, tags.fileNumber.get)
  }
  test("analyze gps") {
    val file = Locations.classpath("20140206_135438_Rue Guimard.jpg")
    val tags: ExifTags = raw.loadExifTags(file)
    //println(tags.tags.toSimpleMap.mkString("\n"))
    //println(tags.gps)
    //println(tags.gps.get.mapHref)
    assertEquals("Some(Gps(50.8436241111111,N,4.36987875,E,0,0,None))", tags.gps.toString)
    val same = Gps("50.8436241111111", "N", "4.36987875", "E", "0", "0")
    assertEquals(same, tags.gps.get)
    val pitesti = Gps("44.860046", "N", "24.867838", "E", "13.0", "0", Some("pitesti"))
    val bucharest = Gps("44.4378258", "N", "26.0946376", "E", "12", "0", Some("bucharest"))
    val brussels = Gps("50.854975", "N", "4.3753899", "E", "12", "0", Some("brussels"))
    assertEquals(107806, pitesti.distanceTo(bucharest).meters.toInt)
    assertEquals(1320, tags.gps.get.distanceTo(brussels).meters.toInt)
    assertEquals(1768176, tags.gps.get.distanceTo(bucharest).meters.toInt)
    assertEquals(1660843, tags.gps.get.distanceTo(pitesti).meters.toInt)
    assertEquals("Brussels", tags.gps.get.closestLocation.name.get)
    assertEquals("Piteşti", pitesti.closestLocation.name.get)
    assertEquals("Bucuresti", bucharest.closestLocation.name.get)
    assertEquals("Brussels", brussels.closestLocation.name.get)
  }
  test("date time in utc using gps") {
    val tags: ExifTags = raw.loadExifTags(Locations.classpath("20140206_135438_Rue Guimard.jpg"))
    println(tags.tags.tags.mkString("\n"))
    val a = tags.gpsDateTimeUTC.get
    assertEquals("2014-02-06T12:54:35.000Z", Formats.extractDateTime("2014:02:06 12:54:35Z").get.toString())
    assertEquals("2014-02-06T12:54:35.000+10:00", Formats.extractDateTime("2014:02:06 12:54:35+10").get.toString())
    assertEquals(new DateTime(2014, 2, 6, 12, 54, 35, 0, DateTimeZone.forOffsetHours(10)), Formats.extractDateTime("2014:02:06 12:54:35+10").get)
    assertEquals("2014-02-06T12:54:35.000Z", tags.gpsDateTimeUTC.get.toString())
    assertEquals("2014-02-06T13:54:38.000", tags.localDateTime.get.toString)
    assertEquals(new LocalDateTime(2014, 2, 6, 13, 54, 38, 0), tags.localDateTime.get)
    assertEquals(DateTimeZone.forOffsetHoursMinutes(1, 0), tags.dateTimeZone.get)
    assertEquals("+01:00", tags.dateTimeZone.get.toString)
    //assertEquals("+01:00",tags.dateTimeZone.get.toTimeZone().getDisplayName())
    assertEquals("2014-02-06T13:54:38.000+01:00", tags.dateTime.get.toString())
    assertEquals("2014-02-06T13:54:35.000+01:00", tags.gpsDateTime.get.toString())
    assertEquals("2014-02-06T13:54:38.000+01:00", tags.getDateTime("dateTime").get.toString())
  }
  test("analyze interpolate with gps") {
    val file = Locations.classpath("20140206_135438_Rue Guimard.jpg")
    val tags: ExifTags = raw.loadExifTags(file)
    assertEquals("2014-02-06--13-54-38------Brussels--Rue Guimard.jpg", tags.interpolate("$exifE36867|$exifModifyDate|$exifDateTimeOriginal(%Y-%m-%d--%H-%M-%S)---$exifFileNumber|(%%)---$compClosestLocation--$compRemaining.$fileExtension").get)
  }
  test("analyze file name containing spaces") {
    raw.loadExifTags(Locations.classpath("a b.jpg"))
  }
  test("analyze duble files") {
    val file = Locations.classpath("MVI_2366.MOV")
    val tags: ExifTags = raw.loadExifTags(file)
    val newName = tags.analyse(file.name).get
    assertEquals("${const:MVI}_${exifFileNumberMinor}.${fileExtension}", newName)
  }
}