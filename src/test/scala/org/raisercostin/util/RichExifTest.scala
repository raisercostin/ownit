package org.raisercostin.util
import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner
import org.raisercostin.util.io.Locations
import org.raisercostin.own.raw

@RunWith(classOf[JUnitRunner])
class RichExifTest extends FunSuite with BeforeAndAfterAll {
  import org.raisercostin.exif._
  import org.raisercostin.exif.RichExif._
  test("extract exif from one file") {
    val file = Locations.classpath("20131008_175240.jpg")
    val result = raw.sanselanExifExtractor(file)
    val result2 = extractExifTags(file.toFile).toSimpleMap
    assertEquals(result.mkString("\n"), result2.mkString("\n"))
    assertEquals(55, result.size)
    assertEquals("", result.mkString("\n"))
  }

  test("interpolate tags") {
    val file = Locations.classpath("20131008_175240.jpg").toFile
    val tags = extractExifTags(file)
    val newName = tags.interpolate("$exifE36867|exifModifyDate|exifDateTimeOriginal(%Y-%m-%d--%H-%M-%S)---$exifFileNumber---$compRemaining.$fileExtension").replaceAll("[-]+[.]", ".")
    assertEquals("2013-10-08--17-52-39.jpg", newName)
  }
  test("analyze format") {
    val file = Locations.classpath("20131008_175240.jpg")
    val tags = extractExifTags(file.toFile)
    val newName = tags.analyze(file.name)
    assertEquals("${exifModifyDate+1s+yyyy}${exifModifyDate+1s+MM}${exifModifyDate+1s+dd}_${exifModifyDate+1s+HH}${exifModifyDate+1s+mm}${exifModifyDate+1s+ss}.jpg", newName)
  }
  test("analyze format2") {
    val file = Locations.classpath("IMG_1558.JPG")
    val tags = extractExifTags(file.toFile)
    assertEquals("aaaa${exifFileNumber}bbb", tags.analyze("aaaa4011558bbb"))
  }
  test("analyze exifFileNumber") {
    val file = Locations.classpath("IMG_1558.JPG")
    val tags = extractExifTags(file.toFile, Seq("IMG"))
    assertEquals("${const:IMG}_${exifFileNumberMinor}.JPG", tags.analyze(file.name))
    assertEquals("", tags.interpolate("$compRemaining"));
  }
  test("exif detected format") {
    val file = Locations.classpath("IMG_1558.JPG").toFile
    val tags = ExifTags(extractExifTags(file))
    assertEquals(1558, tags.fileNumberMinor.get)
    assertEquals(401, tags.fileNumberMajor.get)
    assertEquals(4011558, tags.fileNumber.get)
  }
  test("analyze gps") {
    val file = Locations.classpath("20140206_135438_Rue Guimard.jpg")
    val tags = ExifTags(extractExifTags(file.toFile))
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
  test("analyze interpolate with gps") {
    val file = Locations.classpath("20140206_135438_Rue Guimard.jpg")
    val tags = ExifTags(extractExifTags(file.toFile))
    assertEquals("2014-02-06--13-54-38------Brussels--Rue Guimard.jpg", tags.tags.interpolate("$exifE36867|exifModifyDate|exifDateTimeOriginal(%Y-%m-%d--%H-%M-%S)---$exifFileNumber---$compClosestLocation--$compRemaining.$fileExtension"))
  }
  test("analyze file name containing spaces") {
    extractExifTags(Locations.classpath("a b.jpg").toFile)
  }
  test("analyze duble files") {
    val file = Locations.classpath("MVI_2366.MOV")
    val tags = extractExifTags(file.toFile)
    val newName = tags.analyze(file.name)
    assertEquals("${exifModifyDate+1s+yyyy}${exifModifyDate+1s+MM}${exifModifyDate+1s+dd}_${exifModifyDate+1s+HH}${exifModifyDate+1s+mm}${exifModifyDate+1s+ss}.jpg", newName)
  }
  override def afterAll() {
    close()
  }
}