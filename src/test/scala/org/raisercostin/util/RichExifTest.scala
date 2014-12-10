package org.raisercostin.util
import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner
import org.raisercostin.util.io.Locations

@RunWith(classOf[JUnitRunner])
class LocationsTest extends FunSuite {
  test("extract exif from one file") {
    import org.raisercostin.exif.RichExif._
    val file = Locations.classpath("20131008_175240.jpg").toFile
    val result = extractExifAsMap(file).toSimpleMap
    val result2 = computeMetadata(file).toSimpleMap
    assertEquals(result.mkString("\n"), result2.mkString("\n"))
    assertEquals(55, result.size)
    assertEquals("", result.mkString("\n"))
  }
}