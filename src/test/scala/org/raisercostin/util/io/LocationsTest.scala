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
  
  test("unzip"){
    assertEquals("""ZipInputLocation(ClassPathInputLocation(location.zip),Some(c/))
ZipInputLocation(ClassPathInputLocation(location.zip),Some(c/d.txt))
ZipInputLocation(ClassPathInputLocation(location.zip),Some(a.txt))
ZipInputLocation(ClassPathInputLocation(location.zip),Some(b.txt))
ZipInputLocation(ClassPathInputLocation(location.zip),Some(c/e/))
ZipInputLocation(ClassPathInputLocation(location.zip),Some(c/e/f.txt))""".replaceAll("\r",""), Locations.classpath("location.zip").unzip.list.mkString("\n"))
    assertEquals("""a - file content""", Locations.classpath("location.zip").unzip.child("a.txt").readContent)
    assertEquals("""f content""", Locations.classpath("location.zip").unzip.child("c/e/f.txt").readContent)
    assertEquals("""ZipInputLocation[ClassPathInputLocation(location.zip),Some(c)]""", Locations.classpath("location.zip").unzip.child("c").raw)
    assertEquals("""ZipInputLocation[ClassPathInputLocation(location.zip),Some(c/e)]""", Locations.classpath("location.zip").unzip.child("c").child("e").raw)
    assertEquals("""ZipInputLocation[ClassPathInputLocation(location.zip),Some(c/e/f.txt)]""", Locations.classpath("location.zip").unzip.child("c").child("e").child("f.txt").raw)
    assertEquals("""f content""", Locations.classpath("location.zip").unzip.child("c").child("e").child("f.txt").readContent)
  }
}