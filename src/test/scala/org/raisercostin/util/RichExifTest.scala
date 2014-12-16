package org.raisercostin.util
import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner
import org.raisercostin.util.io.Locations

@RunWith(classOf[JUnitRunner])
class RichExifTest extends FunSuite with BeforeAndAfterAll {
  import org.raisercostin.exif._
  import org.raisercostin.exif.RichExif._
  ignore("extract exif from one file") {
    val file = Locations.classpath("20131008_175240.jpg").toFile
    val result = extractExifAsMap(file).toSimpleMap
    val result2 = extractExifTags(file).toSimpleMap
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
    val tags = extractExifTags(file.toFile)
    //println(tags.toSimpleMap.mkString("\n"))
    assertEquals("IMG_${exifFileNumberMinor}.JPG", tags.analyze(file.name))
  }
  test("exif detected format") {
    val file = Locations.classpath("IMG_1558.JPG").toFile
    val tags = ExifTags(extractExifTags(file))
    //println(tags.tags.toSimpleMap.mkString("\n"))
    assertEquals(1558, tags.fileNumberMinor.get)
    assertEquals(401, tags.fileNumberMajor.get)
    assertEquals(4011558, tags.fileNumber.get)
  }
  test("analyze gps") {
    val file = Locations.classpath("20140206_135438_Rue Guimard.jpg")
    val tags = ExifTags(extractExifTags(file.toFile))
    println(tags.tags.toSimpleMap.mkString("\n"))
    //assertEquals("IMG_${exifFileNumberMinor}.JPG", tags.analyze(file.name))
  }
  test("analyze file name containing spaces") {
    extractExifTags(Locations.classpath("a b.jpg").toFile)
  }
  override def afterAll() {
    close()
  }
}