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

@RunWith(classOf[JUnitRunner])
class RenamerTest extends FunSuite {
  val exif = ExifTags(Tags(Map(
    "key1" -> new org.joda.time.DateTime(2015, 1, 6, 11, 44, 8, 0, DateTimeZone.UTC).toString(Formats.dateTimeInternalExifFormatter),
    "exifFileNumberMajor" -> "437",
    "exifFileNumberMinor" -> "2366",
    "exifMIMEType" -> "image/jpg",
    "fileExtension" -> "jpg")))
  val current = Locations.relative()
  import Renamer._
  val placeBadFiles = current.child("bad")
  val placeGoodFiles = current.child("good")
  //2013-10-26--18-14-15+XXXX---161-IMG_3601---at-XXX.JPG
  val file = Locations.relative("""p1\p2\p3\p4\pic.jpg""")
  def newPath(file: String, name: String = "byYearMonth") = {
    val f = allRenamers.find(_.folder == name).get
    f.proposal(placeBadFiles, placeGoodFiles)(Locations.relative(file), exif).standard(_.relativePath)
  }

  ignore("single") {
    assertEquals("""good/byYearMonth/pic.jpg""", newPath(""))
  }
  test("1 ascendent") {
    assertEquals("""good/byYearMonth/pic.jpg""", newPath("pic.jpg"))
  }
  test("2 ascendents") {
    assertEquals("""good/byYearMonth/p4/pic.jpg""", newPath("p4/pic.jpg"))
  }
  test("3 ascendents") {
    assertEquals("""good/byYearMonth/p3--p4/pic.jpg""", newPath("p3/p4/pic.jpg"))
  }
  test("4 ascendents") {
    assertEquals("""good/byYearMonth/p2--p3--p4/pic.jpg""", newPath("p2/p3/p4/pic.jpg"))
  }
  test("standard Simplified") {
    assertEquals("""good/standardSimplified/2013--2013-XX-XX--old-todelete--2013 - projects/pic.jpg""", newPath("""2013/2013-XX-XX--old-todelete/2013 - projects/pic.jpg""","standardSimplified"))
  }
  test("standard Simplified with known folder name") {
    assertEquals("""good/standardSimplified/2013-07-25--09-50-10+XXXX---106-IMG_0027---at-XXX.JPG""", newPath("""437_2366/2013-07-25--09-50-10+XXXX---106-IMG_0027---at-XXX.JPG""","standardSimplified"))
  }
}