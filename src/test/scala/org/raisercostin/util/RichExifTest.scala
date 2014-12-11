package org.raisercostin.util
import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner
import org.raisercostin.util.io.Locations

@RunWith(classOf[JUnitRunner])
class RichExifTest extends FunSuite {
  import org.raisercostin.exif._
  import org.raisercostin.exif.RichExif._
  test("extract exif from one file") {
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
    val newName = tags.analyze(file.name)
    assertEquals("badIMG_1558.JPG", newName)
  }
  test("exif detected format") {
    val file = Locations.classpath("20131008_175240.jpg").toFile
    val tags = ExifTags(extractExifTags(file))
    assertEquals("${exifModifyDate+1s+yyyy}${exifModifyDate+1s+MM}${exifModifyDate+1s+dd}_${exifModifyDate+1s+HH}${exifModifyDate+1s+mm}${exifModifyDate+1s+ss}", tags.detectedFormat.get.get)
  }
}