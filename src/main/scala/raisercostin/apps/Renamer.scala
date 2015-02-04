package raisercostin.apps
import scala.util.Try
import java.io.File
import scala.util.Success
import scala.util.Failure
import scala.util.matching.Regex
import java.util.regex.Pattern
import scala.collection.immutable.TreeMap
import org.joda.time.DateTimeZone
import org.apache.commons.io.filefilter.RegexFileFilter
import org.raisercostin.util.io.Locations
import org.raisercostin.exif.ExifTags
import org.raisercostin.tags.raw
import org.raisercostin.util.io.FileLocation
import org.raisercostin.tags.FormatAnalyser._
import org.raisercostin.util.io.OutputLocation

object Renamer {
  def main(args: Array[String]) = {
    //test
    //main2(args)
    //ownPics("""d:\personal\work\conta\brainlight&pfa\440_0111\""","""proposed1""")
    //ownPics("""d:\personal\photos-tofix\photos-old-todelete\""", """-proposed2""")
    //ownPics("""d:\personal\photos-tofix\2013-proposed1-bad\""","""-proposed3""")
    ownPics("""d:\personal\photos-problematic\""", """-proposed1""")
    //System.exit(0)
  }
  def main2(args: Array[String]) = {
    args match {
      case Array(from: String, to: String) =>
        ownPics(from, to)
      case Array(from: String) =>
        dumpInfo(from)
      case _ =>
        println(s"""You must give two parameters and you gave ${args.toList.mkString("\n")}. \nThe folder (that will NEVER be changed) with your media(pics,movies) files and the folder where you want to get a proposal of new names based on EXIF information.""")
    }
    println("done.")
  }

  def test = {
    //ownPics("""d:\personal\photos-tofix\photos-2014-12-18\""","""proposed1""")
    //ownPics("""d:\personal\photos""","""d:\personal\photos-proposed1""")
    //ownPics("""d:\personal\photos\2013-XX-XX\""","""d:\proposed2""")
    //ownPics(""".\test\special11""","""d:\proposed11""")
    //ownPics("""D:\personal\work\ownit\.\test\special6\1980-01-01--00-00-10---MVI_1723.AVI""","""d:\proposed3""")
    //ownPics("""D:\personal\photos\2013-XX-XX\108_0731""","""d:\proposed4""",Some("*IMG*0043*"))
    //ownPics("""z:\master\test""", """z:\master\test-proposed""")
  }
  def dumpInfo(file: String) = {
    println(raw.loadExifTags(Locations.file(file)).tags.tags.mkString("\n"))
  }

  case class DevicesDao(var allDevices: Set[String], toDevices: FileLocation) {
    def checkDeviceId(tags: ExifTags) = {
      val device = tags.deviceId
      if (!device.isDefined) {
        println(tags.tags.tags.mkString("\n"))
      }
      if (device.isDefined && !allDevices.contains(device.get)) {
        allDevices += device.get
        toDevices.appendContent(device.get + "\n")
      }
    }
  }

  def ownPics(fromPath: String, toRelativeOrAbsolute: String, filter: Option[String] = None) = try {
    val from = Locations.file(fromPath)
    val toInitial = Locations.file(toRelativeOrAbsolute)
    val to = if (toInitial.isAbsolute) toInitial else from.withName(_ + toRelativeOrAbsolute)
    println("rename from " + from + " to " + to)
    val placeBadFiles = to.withName(_ + "-bad")
    val placeGoodFiles = to.withName(_ + "-good")
    implicit val allDevices = DevicesDao(Set(), placeGoodFiles.withBaseName(_ + "-devices").renamedIfExists)
    println(from.traverse.filter {
      case (file1, x) =>
        val file = file1.toFile
        filter.isEmpty || fileWildcard(filter.get, file.getAbsolutePath)
    }.map {
      case (file1, x) =>
        val file = file1.toFile
        val src = Locations.file(file)
        process(src, from, placeBadFiles, placeGoodFiles)
    }.filter(_.isFailure).map {
      case Failure(f) => dump(f)
    }.mkString("\n"))
  }

  def process(src: FileLocation, from: FileLocation, placeBadFiles: FileLocation, placeGoodFiles: FileLocation)(implicit devices: DevicesDao) = {
    println("analyze " + src.absolute + " ...")
    val newName = Try {
      val tags = raw.loadExifTags(src)
      devices.checkDeviceId(tags)
      println("detected " + tags.analyse(src.relativeTo(src.parent.parent)))
      val allRenamers = Seq(("flat", flat _), ("standard", standardizeName _),("byYear",byYear _), ("byYearMonth",byYearAndMonth _), ("byCounter",byCounter _))
      allRenamers.foreach { case (name,renamer) =>
        val newDestFile = renamer(placeBadFiles.withBaseName(_ + "-" + name), placeGoodFiles.withBaseName(_ + "-" + name))(from, src, tags)
        val ANSI_BACK = "" //"\u001B[1F";
        println(ANSI_BACK + "\t"+name+"\t> smartcopy  " + src + " to " +
          newDestFile.name + "\t\tdetectedFormat:" + tags.tags.analyse(src.name) + "\t" + newDestFile)
        src.copyAsHardLink(newDestFile)
      }
    }
    newName.recover {
      case e =>
        e.printStackTrace
        placeBadFiles.child(src.relativeTo(from)).mkdirOnParentIfNecessary.copyFromAsHardLink(src)
        Failure(e)
    }
    newName
  }

  def byCounter(placeBadFiles: FileLocation, placeGoodFiles: FileLocation)(from: FileLocation, src: FileLocation, tags: ExifTags): FileLocation = {
    val newName = tags.interpolate("$exifFileNumberMajor(%%)|(XXX)-$exifFileNumberMinor(%%)|(XXXX)$compRemaining|(-%%|)$fileExtension(.%%)").get
    placeGoodFiles.child(newName).mkdirOnParentIfNecessary.renamedIfExists
  }
  def byYearAndMonth(placeBadFiles: FileLocation, placeGoodFiles: FileLocation)(from: FileLocation, src: FileLocation, tags: ExifTags): FileLocation = {
    val newName = tags.interpolate(tags.dateGroup+"(yyyy)|(XXXX)-"+tags.dateGroup+"(MM-MMMM)|(XX)"+"\\"+dateAnalyser + "---$exifFileNumberMajor|(%%|XXX)-IMG_$exifFileNumberMinor|(%%|XXXX)---at-$compClosestLocation|(%%|XXX)$compRemaining|(--%%|)$fileExtension(.%%)").get
    placeGoodFiles.child(newName).mkdirOnParentIfNecessary.renamedIfExists
  }
  def byYear(placeBadFiles: FileLocation, placeGoodFiles: FileLocation)(from: FileLocation, src: FileLocation, tags: ExifTags): FileLocation = {
    val newName = tags.interpolate(tags.dateGroup+"(yyyy)|(XXXX)\\"+dateAnalyser + "---$exifFileNumberMajor|(%%|XXX)-IMG_$exifFileNumberMinor|(%%|XXXX)---at-$compClosestLocation|(%%|XXX)$compRemaining|(--%%|)$fileExtension(.%%)").get
    placeGoodFiles.child(newName).mkdirOnParentIfNecessary.renamedIfExists
  }
  def flat(placeBadFiles: FileLocation, placeGoodFiles: FileLocation)(from: FileLocation, src: FileLocation, tags: ExifTags): FileLocation = {
    val newName = tags.interpolate(dateAnalyser + "---$exifFileNumberMajor|(%%|XXX)-IMG_$exifFileNumberMinor|(%%|XXXX)---at-$compClosestLocation|(%%|XXX)$compRemaining|(--%%|)$fileExtension(.%%)").get
    placeGoodFiles.mkdirIfNecessary.child(newName).renamedIfExists
  }
  def standardizeName(placeBadFiles: FileLocation, placeGoodFiles: FileLocation)(from: FileLocation, src: FileLocation, tags: ExifTags): FileLocation = {
    //val newName = tags.interpolate("$exifE36867|exifModifyDate|exifDateTimeOriginal|fileModification(%Y-%m-%d--%H-%M-%S)---$compRemaining.$fileExtension").replaceAllLiterally("---.", ".")
    //val newName = tags.interpolate("$dateTime|$exifE36867|$exifModifyDate#THM|$exifModifyDate|$exifDateTimeOriginal#THM|$exifDateTimeOriginal|(%Y-%m-%d--%H-%M-%SZ|XXXX-XX-XX--XX-XX-XX)---$exifFileNumberMajor|(%%|XXX)-IMG_$exifFileNumberMinor|(%%|XXXX)---at-$compClosestLocation|(%%|XXX)$compRemaining|(--%%|)$fileExtension(.%%)").get
    //val newName = tags.interpolate(dateAnalyser+"---$exifFileNumberMajor|(%%|XXX)-IMG_$exifFileNumberMinor|(%%|XXXX)---$exifImageWidth$exifImageHeight(x%%)---at-$compClosestLocation|(%%|XXX)$compRemaining|(--%%|)$fileExtension(.%%)").get
    val newName = tags.interpolate(dateAnalyser + "---$exifFileNumberMajor|(%%|XXX)-IMG_$exifFileNumberMinor|(%%|XXXX)---at-$compClosestLocation|(%%|XXX)$compRemaining|(--%%|)$fileExtension(.%%)").get
    val imageOrVideo = tags.isImage || tags.isVideo
    val badName = newName.contains("XXXX-XX-XX--XX-XX-XX")
    val badChange = badName && imageOrVideo
    val nameChanged = !badName && imageOrVideo
    val dest = if (badChange) placeBadFiles else placeGoodFiles
    val baseName = if (nameChanged) newName else src.name
    val destFile = dest.child(src.relativeTo(from)).withName(_ => baseName).mkdirOnParentIfNecessary
    val newDestFile = destFile.renamedIfExists
    newDestFile
  }

  def fileWildcard(filter: String, file: String): Boolean = {
    val regex = "^" + filter.replace("?", ".?").replace("*", ".*?") + "$"
    file.matches(regex)
  }
  def dump(t: Throwable): String = {
    val a = t.toString + " Stacktrace:\n" + t.getStackTraceString
    if (t.getCause != null) {
      a + "caused by " + dump(t.getCause())
    } else {
      a
    }
  }

  def main2(filename: String) {
    val file = Locations.file(filename).toFile
    println(file.getAbsolutePath)

    import org.apache.sanselan.{ ImageReadException, Sanselan }
    import org.apache.sanselan.common.{ IImageMetadata, RationalNumber }
    import org.apache.sanselan.formats.jpeg.JpegImageMetadata
    import org.apache.sanselan.formats.tiff.{ TiffField, TiffImageMetadata }
    import org.apache.sanselan.formats.tiff.constants.{ ExifTagConstants, GPSTagConstants, TagInfo, TiffConstants, TiffTagConstants }
    import scala.collection.JavaConversions._
    val metadata = Sanselan.getMetadata(file)
      def printTagValue(metadata: JpegImageMetadata, tagInfo: TagInfo): Unit = {
        val field = metadata.findEXIFValue(tagInfo)
        field match {
          case null => println("        (" + tagInfo.name + " not found.)")
          case _ => println("        " + tagInfo.name + ": " + field.getValueDescription())
        }
      }

    println("file:" + file.getPath())

    if (metadata == null) {
      println("\tNo EXIF metadata was found")
    }

    if (metadata.isInstanceOf[JpegImageMetadata]) {
      val jpegMetadata = metadata.asInstanceOf[JpegImageMetadata]

      println("  -- Standard EXIF Tags")
      printTagValue(jpegMetadata, TiffTagConstants.TIFF_TAG_XRESOLUTION)
      printTagValue(jpegMetadata, TiffTagConstants.TIFF_TAG_DATE_TIME)
      printTagValue(jpegMetadata, ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)
      printTagValue(jpegMetadata, ExifTagConstants.EXIF_TAG_CREATE_DATE)
      printTagValue(jpegMetadata, ExifTagConstants.EXIF_TAG_ISO)
      printTagValue(jpegMetadata, ExifTagConstants.EXIF_TAG_SHUTTER_SPEED_VALUE)
      printTagValue(jpegMetadata, ExifTagConstants.EXIF_TAG_APERTURE_VALUE)
      printTagValue(jpegMetadata, ExifTagConstants.EXIF_TAG_BRIGHTNESS_VALUE)
      printTagValue(jpegMetadata, GPSTagConstants.GPS_TAG_GPS_LATITUDE_REF)
      printTagValue(jpegMetadata, GPSTagConstants.GPS_TAG_GPS_LATITUDE)
      printTagValue(jpegMetadata, GPSTagConstants.GPS_TAG_GPS_LONGITUDE_REF)
      printTagValue(jpegMetadata, GPSTagConstants.GPS_TAG_GPS_LONGITUDE)

      // simple interface to GPS data
      println("  -- GPS Info")
      val exifMetadata = jpegMetadata.getExif()
      if (exifMetadata != null) {
        val gpsInfo = exifMetadata.getGPS()
        if (gpsInfo != null) {
          val longitude = gpsInfo.getLongitudeAsDegreesEast()
          val latitude = gpsInfo.getLatitudeAsDegreesNorth()

          println("        GPS Description: " + gpsInfo)
          println("        GPS Longitude (Degrees East): " + longitude)
          println("        GPS Latitude (Degrees North): " + latitude)
        }
      }

      println("  -- All EXIF info")
      jpegMetadata.getItems().foreach(item => println("        " + item))
      println("")
    }
  }
}