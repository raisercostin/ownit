package org.raisercostin.util.io
import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LocationsTest extends FunSuite {
  test("bug - test spaces in classpath filename") {
    val file = Locations.classpath("a b.jpg")
    assertEquals("a b.jpg", file.resourcePath)
    assertEquals("/a%20b.jpg", file.toUrl.toString().takeRight(10))
    assertEquals("/a%20b.jpg", file.toUrl.toExternalForm().takeRight(10))
    assertEquals("/a%20b.jpg", file.toUrl.toURI().toString().takeRight(10))
    assertEquals("/a%20b.jpg", file.toUrl.toURI().toURL().toString().takeRight(10))
    assertEquals("/a%20b.jpg", file.toUrl.toURI().toASCIIString().takeRight(10))
    assertEquals("a%20b.jpg", new java.io.File(file.toUrl.getFile()).getName())
    assertEquals("a b.jpg", new java.io.File(file.toUrl.toURI()).getName())
    assertEquals("a b.jpg", file.toFile.getName())
    assertEquals("a b.jpg", file.absolute.takeRight(7))
    assertEquals("a b.jpg", file.name)
  }
}