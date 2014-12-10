package org.raisercostin.util
import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.raisercostin.exif.RichExif.extractExifAsMap
import org.raisercostin.exif.RichExif.toSimpleMap
import org.scalatest.junit.JUnitRunner
import org.raisercostin.util.io.Locations

@RunWith(classOf[JUnitRunner])
class LocationsTest extends FunSuite {
  test("extract exif from one file") {
    import org.raisercostin.exif.RichExif._
    val result = toSimpleMap(extractExifAsMap(Locations.classpath("20131008_175240.jpg").toFile))
    assertEquals(55, result.size)
    assertEquals("", result.mkString("\n"))
  }
}