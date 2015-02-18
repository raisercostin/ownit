package org.raisercostin.apps

import org.joda.time.DateTimeZone
import org.junit.Assert.assertEquals
import org.junit.runner.RunWith
import org.raisercostin.exif.ExifTags
import org.raisercostin.tags.Formats
import org.raisercostin.tags.Tags
import org.raisercostin.util.io.Locations
import org.scalatest.FunSuite
import Renamer.AnalysedFolder
import Renamer.Formatter
import Renamer.standardName
import org.scalatest.junit.JUnitRunner
import org.raisercostin.util.io.FileLocation
import org.raisercostin.util.io.NavigableLocation
import org.raisercostin.tags.raw
import org.raisercostin.util.io.InputLocation
import org.joda.time.DateTime

@RunWith(classOf[JUnitRunner])
class RenamerTest extends FunSuite {
  import org.scalatest.Matchers._
  val defaultExif = Tags(Map(
    "key1" -> new org.joda.time.DateTime(2013, 8, 1, 10, 29, 9, 0, DateTimeZone.UTC).toString(Formats.dateTimeInternalExifFormatter),
    "exifFileNumberMajor" -> "437",
    "exifFileNumberMinor" -> "2366",
    "exifMIMEType" -> "image/jpg",
    "fileExtension" -> "jpg"))
  val current = Locations.relative()
  import Renamer._
  val placeBadFiles = current.child("bad")
  val placeGoodFiles = current.child("good")
  //2013-10-26--18-14-15+XXXX---161-IMG_3601---at-XXX.JPG
  val file = Locations.relative("""p1\p2\p3\p4\pic.jpg""")
  def newPath(src: InputLocation): String = {
    val tags = raw.loadExifTags(src)
    newPath(src.raw, exif = tags.tags)
  }
  def newPath(file: String, name: String = "byYearMonth", exif: Tags = defaultExif): String = {
    val f = allRenamers.find(_.folder == name).get
    val src = Locations.relative(file)
    f.proposal(placeBadFiles, placeGoodFiles)(src, ExifTags(exif ++ Map("exifFileName" -> src.name, "fileExtension" -> src.extension))).standard(_.relativePath)
  }

  ignore("single") {
    assertEquals("""good/byYearMonth/pic.jpg""", newPath(""))
  }
  test("1 ascendent") {
    assertEquals("""good/byYearMonth/---437-IMG_2366--pic.jpg""", newPath("pic.jpg"))
  }
  test("1 ascendent and unknown file type") {
    assertEquals("""good/byYearMonth/unorganized/.picasa.ini""", newPath(".picasa.ini", exif = Tags(Map())))
  }
  test("2 ascendents") {
    assertEquals("""good/byYearMonth/pA/---437-IMG_2366--pic.jpg""", newPath("pA/pic.jpg"))
  }
  test("3 ascendents") {
    assertEquals("""good/byYearMonth/pB--pA/---437-IMG_2366--pic.jpg""", newPath("pB/pA/pic.jpg"))
  }
  test("4 ascendents") {
    assertEquals("""good/byYearMonth/pC--pB--pA/---437-IMG_2366--pic.jpg""", newPath("pC/pB/pA/pic.jpg"))
  }
  test("standard Simplified") {
    assertEquals("""good/standardSimplified/projects/---437-IMG_2366--pic.jpg""", newPath("""2013/2013-XX-XX--old-todelete/2013 - projects/pic.jpg""", "standardSimplified"))
  }
  test("standard Simplified with known folder name") {
    assertEquals("""good/standardSimplified/---437-IMG_2366--2013-07-25--09-50-10--106-0027.jpg""", newPath("""437_2366/2013-07-25--09-50-10+XXXX---106-IMG_0027---at-XXX.jpg""", "standardSimplified"))
  }
  test("byYearMonth date detection in path") {
    assertEquals("""good/byYearMonth/2013-08-August/2013-08-01--10-29-09+0000---437-IMG_2366.JPG""",
      newPath("""2013/2013-08-01/DCIM/437_0801/2013-08-01--10-29-09+XXXX---437-IMG_2366---at-XXX.JPG""", "byYearMonth", defaultExif ++ Map("dateTime" -> defaultExif.getString("key1").get)))
  }
  test("bug1 - no folder with date") {
    assertEquals("""good/byYearMonth/2012-01-January--a/2012-01-17--13-09-44--14.jpg""", newPath(Locations.classpath("a/2012-01-17 14.09.45.jpg")))
    //deduct timezone from difference in date/time in name and and in timestamp
  }
  test("find proper version date") {
    assertEquals(DateTime.parse("2002-02-01T00:00:00.000Z"), ExifTags.availabilityDate("0220").get)
    assertEquals(DateTime.parse("1998-12-01T00:00:00.000Z"), ExifTags.availabilityDate("0219").get)
    assertEquals(None, ExifTags.availabilityDate("0099"))
    assertEquals(DateTime.parse("1995-10-01T00:00:00.000Z"), ExifTags.availabilityDate("0100").get)
    assertEquals(DateTime.parse("2003-09-01T00:00:00.000Z"), ExifTags.availabilityDate("0222").get)
  }
  test("bug2 - broken exif?") {
    val src = Locations.classpath("a/a.jpg")
    val tags = raw.loadExifTags(src)
    println(tags.tags.tags.mkString("\n"))
    assertEquals("0220", tags.exifVersion.get)
    assertEquals(DateTime.parse("2002-02-01T00:00:00.000Z"), tags.exifVersionDate.get)
    assert(tags.originalExif)
    //assertEquals(None,tags.validDateTime(DateTime.parse("1980-02-01T00:00:00.000Z")))
    //assertEquals(DateTime.parse("2002-02-01T00:00:00.000Z"),tags.validDateTime(DateTime.parse("2002-02-01T00:00:00.000Z")).get)
    //if has a commonly used exif data but before exif standard like: 
    assertEquals("""good/byYearMonth/1980-01-January--a/1980-01-01--00-00-03---108-IMG_3550--a.jpg""", newPath(src))
  }
}