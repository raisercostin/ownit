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

object Renamer {
  def main(args: Array[String]) = {
    args match {
      case Array(from: String, to: String) =>
        ownPics(from, to)
      case Array(from: String) =>
        dumpInfo(from)
      case _ =>
        println(s"""You must give two parameters and you gave ${args.toList.mkString("\n")}. \nThe folder (that will NEVER be changed) with your media(pics,movies) files and the folder where you want to get a proposal of new names based on EXIF information.""")
    }
  }

  def test = {
    //ownPics("""d:\personal\photos\201X-XX-XX\""","""d:\proposed1""")
    //ownPics("""d:\personal\photos\2013-XX-XX\""","""d:\proposed2""")
    //ownPics(""".\test\special11""","""d:\proposed11""")
    //ownPics("""D:\personal\work\ownit\.\test\special6\1980-01-01--00-00-10---MVI_1723.AVI""","""d:\proposed3""")
    //ownPics("""D:\personal\photos\2013-XX-XX\108_0731""","""d:\proposed4""",Some("*IMG*0043*"))
    ownPics("""z:\master\test""", """z:\master\test-proposed""")
  }
  def dumpInfo(file: String) = {
    import org.raisercostin.exif.RichExif._
    println(extractExifAsMap(Locations.file(file).toFile).toSimpleMap.mkString("\n"))
  }

  import org.raisercostin.exif.RichExif
  def ownPics(fromPath: String, toRelativeOrAbsolute: String, filter: Option[String] = None) = try {
    //main2(if(args.isEmpty) """d:\personal\photos\_desene\""" else args(0))
    //main2(""".\photo04.jpg""")
    //println(RichExif.formatIrfanView(""".\photo04.jpg""", "$E36867(%Y-%m-%d--%H-%M-%S)--$F"))
    //    main2(""".\20140617_191316_Bulevardul Mircea Vodă.jpg""")
    //println(RichExif.formatIrfanView(""".\20140617_191316_Bulevardul Mircea Vodă.jpg""", "$E36867(%Y-%m-%d--%H-%M-%S)--$F"))

    //d:\personal\photos\
    //val from = """.\test\special3"""
    //val from = """d:\personal\photos\2014-XX-XX\"""
    //val from = """d:\personal\work\raisercostin-utils\test\other\00006.MTS"""
    //val from = """d:\_good"""
    //val to = """d:\proposed"""
    //val place = """.\test\20131008_175240.jpg"""
    val from = Locations.file(fromPath)
    val toInitial = Locations.file(toRelativeOrAbsolute)
    val to = if (toInitial.isAbsolute) toInitial else from.withName(_ + toRelativeOrAbsolute)
    println("rename from " + from + " to " + to)
    val placeBadFiles = to.withName(_ + "-bad")
    val placeGoodFiles = to.withName(_ + "-good")
    println(from.traverse.filter {
      case (file1, x) =>
        val file = file1.toFile
        filter.isEmpty || fileWildcard(filter.get, file.getAbsolutePath)
    }.map {
      case (file1, x) =>
        val file = file1.toFile
        println("analyze " + file.getAbsolutePath + " ...")
        val src = Locations.file(file)
        val newName =
          //          if (src.name == ".picasa.ini")
          //            Try(src.name)
          //          else
          Try {
            //println(file + ":" + RichExif.extractExifAsMap(file).mkString("\n"))
            //RichExif.formatIrfanView(file, "$E36867(%Y-%m-%d--%H-%M-%S)---$F") + " has format " + RichExif.extractFormat(file) + " remaining:" + remainingFormat(file))
            val metadata = RichExif.computeMetadata(file)
            //println("attributes " + file + " : \n" + (toSimpleMap(metadata).mkString("\n")))
            //val newName = RichExif.format(metadata, "$exifE36867|exifModifyDate|exifDateTimeOriginal|fileModification(%Y-%m-%d--%H-%M-%S)---$compRemaining.$fileExtension").replaceAllLiterally("---.", ".")
            val newName = metadata.interpolate("$exifE36867|exifModifyDate|exifDateTimeOriginal(%Y-%m-%d--%H-%M-%S)---$exifFileNumber---$compRemaining.$fileExtension").replaceAll("[-]+[.]", ".")
            val ANSI_BACK = "" //"\u001B[1F";
            println(ANSI_BACK + "rename  " + file + " to " +
              newName + "\t\tdetectedFormat:" + metadata.vars.get(RichExif.tagCompDetectedFormat).map(_.apply("")).getOrElse(""))

            val extensionsWithExif = Set("jpg", "jpeg", "gif", "mp4", "avi", "png", "bmp")
            val badChange = newName.contains("%H-%M-%S") && extensionsWithExif.contains(src.extension.toLowerCase)
            val nameChanged = !newName.contains("%H-%M-%S")
            val dest = if (badChange) placeBadFiles else placeGoodFiles
            val baseName = if (nameChanged) newName else src.name
            val destFile = dest.child(src.relativeTo(from)).withName(_ => baseName).mkdirOnParentIfNecessary
            var newDestFile = destFile
            var counter = 1
            while (newDestFile.exists) {
              newDestFile = destFile.withBaseName(baseName => baseName + "-" + counter)
              counter += 1
            }
            newDestFile.copyFromAsHardLink(src)
            newName
          }
        newName.recover {
          case e =>
            placeBadFiles.child(src.relativeTo(from)).mkdirOnParentIfNecessary.copyFromAsHardLink(src)
            Failure(e)
        }
        newName
    }.filter(_.isFailure).map {
      case Failure(f) => dump(f)
    }.mkString("\n"))
  } finally {
    RichExif.close
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