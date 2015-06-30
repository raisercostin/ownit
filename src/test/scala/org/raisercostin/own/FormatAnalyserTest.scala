package org.raisercostin.own

import org.raisercostin.tags.Formats
import org.scalatest._
import org.junit.runner.RunWith
import org.junit.Assert._
import org.scalatest.junit.JUnitRunner
import org.raisercostin.tags.FormatAnalyser
import org.raisercostin.tags.FormatInterpolator
import org.raisercostin.tags.Tags

@RunWith(classOf[JUnitRunner])
class FormatAnalyserTest extends FunSuite with BeforeAndAfterAll with TryValues {
  val tags2 = Map(
    "exifDateTimeOriginal" -> new org.joda.time.DateTime(2015, 1, 6, 11, 44, 8, 0).toString(Formats.dateTimeInternalExifFormatter),
    "exifFileNumberMajor" -> "437",
    "exifFileNumberMinor" -> "2366",
    "fileExtension" -> "THM")
  val analyse = FormatAnalyser(tags2)

  test("analyse key with const") {
    assertEquals("${const:IMG}_${exifFileNumberMinor}", analyse("IMG_2366").get)
  }
  ignore("analyse with formatter", Tag("failed")) {
    assertEquals("$exifFileNumberMinor(IMG_%%)", analyse("IMG_2366").get)
  }
  test("test analyser") {
    assertEquals("${exifFileNumberMajor}", analyse("437").get)
    assertEquals("${exifFileNumberMinor}", analyse("2366").get)
    assertEquals("${exifFileNumberMajor}/${const:IMG}_${exifFileNumberMinor}.thm", analyse("437/IMG_2366.thm").get)
  }
  test("analyze date format") {
    assertEquals("${exifDateTimeOriginal+yyyy}${exifDateTimeOriginal+MM}${exifDateTimeOriginal+dd}_${exifDateTimeOriginal+HH}${exifDateTimeOriginal+mm}${exifDateTimeOriginal+ss}.jpg", analyse("20150106_114408.jpg").get)
    assertEquals("${exifDateTimeOriginal+1s+yyyy}${exifDateTimeOriginal+1s+MM}${exifDateTimeOriginal+1s+dd}_${exifDateTimeOriginal+1s+HH}${exifDateTimeOriginal+1s+mm}${exifDateTimeOriginal+1s+ss}.jpg", analyse("20150106_114409.jpg").get)
    assertEquals("${exifDateTimeOriginal+2s+yyyy}${exifDateTimeOriginal+2s+MM}${exifDateTimeOriginal+2s+dd}_${exifDateTimeOriginal+2s+HH}${exifDateTimeOriginal+2s+mm}${exifDateTimeOriginal+2s+ss}.jpg", analyse("20150106_114410.jpg").get)
    assertEquals("${exifDateTimeOriginal+3s+yyyy}${exifDateTimeOriginal+3s+MM}${exifDateTimeOriginal+3s+dd}_${exifDateTimeOriginal+3s+HH}${exifDateTimeOriginal+3s+mm}${exifDateTimeOriginal+3s+ss}.jpg", analyse("20150106_114411.jpg").get)
    assertEquals("${exifDateTimeOriginal+yyyy}${exifDateTimeOriginal+MM}${exifDateTimeOriginal+dd}_${exifDateTimeOriginal+HH}${exifDateTimeOriginal+mm}12.jpg", analyse("20150106_114412.jpg").get)
  }
  ignore("analyze consolidated date format", Tag("failed")) {
    assertEquals("$exifDateTimeOriginal(yyyyMMdd_HHmmss).jpg", analyse("20150106_114408.jpg").get)
  }
  test("analyze format2") {
    assertEquals("aaaa${exifFileNumberMajor}${exifFileNumberMinor}bbb", analyse("aaaa4372366bbb").get)
  }
  test("analyze format using interpolator") {
    assertEquals("Success(2015:01:06 11:44:08+0200)", FormatInterpolator(tags2)(FormatAnalyser.dateAnalyserNoFormat).toString)
    assertEquals("$exifFileNumberMajor_$dateTime|$localDateTime|$exifE36867|$exifDateTimeOriginal#${fileExtension}|$exifDateTimeOriginal|$pathLocalDateTime|$exifCreateDate|$exifModifyDate#THM|$exifModifyDate(MMdd).jpg", analyse("437_0106.jpg").get)
  }
  test("exclude tags from analisys") {
    val tags = Tags(tags2)
    assertEquals("aaaa${exifFileNumberMajor}${exifFileNumberMinor}bbb", tags.analyse("aaaa4372366bbb", Seq()).get)
    assertEquals("aaaa437${exifFileNumberMinor}bbb", tags.analyse("aaaa4372366bbb", Seq("exifFileNumberMajor")).get)
    assertEquals("aaaa${exifFileNumberMajor}2366bbb", tags.analyse("aaaa4372366bbb", Seq("exifFileNumberMinor")).get)
    assertEquals("aaaa4372366bbb", tags.analyse("aaaa4372366bbb", Seq("exifFileNumberMinor", "exifFileNumberMajor")).get)
  }
}

