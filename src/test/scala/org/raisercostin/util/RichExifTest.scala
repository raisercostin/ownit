package org.raisercostin.util
import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner
import org.raisercostin.util.io.Locations

@RunWith(classOf[JUnitRunner])
class RichExifTest extends FunSuite {
  test("extract exif from one file") {
    import org.raisercostin.exif.RichExif._
    val file = Locations.classpath("20131008_175240.jpg").toFile
    val result = extractExifAsMap(file).toSimpleMap
    val result2 = extractExifTags(file).toSimpleMap
    assertEquals(result.mkString("\n"), result2.mkString("\n"))
    assertEquals(55, result.size)
    assertEquals("", result.mkString("\n"))
  }
  
  test("interpolate exif") {
    import org.raisercostin.exif.RichExif._
    val file = Locations.classpath("20131008_175240.jpg").toFile
    val tags = extractExifTags(file)
    val newName = tags.interpolate("$exifE36867|exifModifyDate|exifDateTimeOriginal(%Y-%m-%d--%H-%M-%S)---$exifFileNumber---$compRemaining.$fileExtension").replaceAll("[-]+[.]", ".")
    assertEquals("2013-10-08--17-52-39.jpg", newName)
  }
}