package raisercostin.apps

import scala.util.Try
import java.io.File
import util.io.Locations
import scala.util.Success
import scala.util.Failure
import scala.util.matching.Regex
import java.util.regex.Pattern
import scala.collection.immutable.TreeMap
import org.joda.time.DateTimeZone

object Renamer {
  def main(args: Array[String]) {
    import RichExif._
    //main2(if(args.isEmpty) """d:\personal\photos\_desene\""" else args(0))
    //main2(""".\photo04.jpg""")
    //println(RichExif.formatIrfanView(""".\photo04.jpg""", "$E36867(%Y-%m-%d--%H-%M-%S)--$F"))
    //    main2(""".\20140617_191316_Bulevardul Mircea Vodă.jpg""")
    //println(RichExif.formatIrfanView(""".\20140617_191316_Bulevardul Mircea Vodă.jpg""", "$E36867(%Y-%m-%d--%H-%M-%S)--$F"))
    import util.io.Locations
    //1
    //val from = """d:\personal\photos\2013-XX-XX\"""
    //val to = """d:\proposed"""

    //2
    val from = """.\test\special5"""
    val to = """d:\proposed2"""

      //d:\personal\photos\
    //val from = """.\test\special3"""
    //val from = """d:\personal\photos\2014-XX-XX\"""
    //val from = """d:\personal\work\raisercostin-utils\test\other\00006.MTS"""
    //val from = """d:\_good"""
    //val to = """d:\proposed"""
    //val place = """.\test\20131008_175240.jpg"""
    val placeBadFiles = to + """-bad"""
    val placeGoodFiles = to + """-good"""
    val placeLocation = Locations.file(from)
    println(placeLocation.traverse.map {
      case (file1, x) =>
        val file = file1.toFile
        println("analyze " + file.getAbsolutePath + " ...")
        val src = Locations.file(file)
        val newName = Try {
          //println(file + ":" + RichExif.extractExifAsMap(file).mkString("\n"))
          //RichExif.formatIrfanView(file, "$E36867(%Y-%m-%d--%H-%M-%S)---$F") + " has format " + RichExif.extractFormat(file) + " remaining:" + remainingFormat(file))
          val metadata = RichExif.computeMetadata(file)
          //println("attributes " + file + " : \n" + (toSimpleMap(metadata).mkString("\n")))
          //val newName = RichExif.format(metadata, "$exifE36867|exifModifyDate|exifDateTimeOriginal|fileModification(%Y-%m-%d--%H-%M-%S)---$compRemaining.$fileExtension").replaceAllLiterally("---.", ".")
          val newName = RichExif.format(metadata, "$exifE36867|exifModifyDate|exifDateTimeOriginal(%Y-%m-%d--%H-%M-%S)---$compRemaining.$fileExtension").replaceAllLiterally("---.", ".")
          val ANSI_BACK = "" //"\u001B[1F";
          println(ANSI_BACK + "rename  " + file + " to " +
            newName + "\t\tdetectedFormat:" + metadata.get(tagCompDetectedFormat).flatMap(_.apply("")).getOrElse(""))
          newName
        }
        val goodChange = newName.isSuccess && !newName.get.contains("%H-%M-%S")
        val dest = Locations.file(if (goodChange) placeGoodFiles else placeBadFiles)
        val baseName = if (goodChange) newName.get else src.name
        dest.child(src.relativeTo(placeLocation)).withName(_ => baseName).mkdirOnParentIfNecessary.copyFromAsHardLink(src)
        newName
    }.filter(_.isFailure).map {
      case Failure(f) => dump(f)
    }.mkString("\n"))
  }
  def dump(t: Throwable): String = {
    val a = t.toString + " Stacktrace:\n" + t.getStackTraceString
    if (t.getCause != null) {
      a + "caused by " + dump(t.getCause())
    } else {
      a
    }
  }
  object RichExif {
    type MetadataResult = Option[String]
    type MetadataProvider = (String) => MetadataResult
    type MetadataMap = Map[String, MetadataProvider]

    val tagFileModificationDateTime = "fileModification"
    val tagFileCreated = "fileCreated"
    val tagFileExtension = "fileExtension"
    val tagCompRemaining = "compRemaining"
    val tagCompDetectedFormat = "compDetectedFormat"

    private val exifDateTimeFormatter = org.joda.time.format.DateTimeFormat.forPattern("yyyy:MM:dd HH:mm:ss")
    private val exifDateTimeFormatter2 = org.joda.time.format.DateTimeFormat.forPattern("yyyy:MM:dd HH:mm:ssZ")
    private val exifDateTimeFormatter3 = org.joda.time.format.DateTimeFormat.forPattern("yyyy:00:00 00:00:00")
    private val dateFormat = "yyyy-MM-dd--HH-mm-ss-ZZ"
    def format(metadata: MetadataMap, pattern: String): String = {
      interpolate(pattern, metadata)
    }
    //see http://dcsobral.blogspot.ro/2010/01/string-interpolation-in-scala-with.html
    def interpolate(text: String, vars: MetadataMap) = {
      import scala.util.matching.Regex
      var result = """\$\{([^}]+)\}""".r.replaceAllIn(text, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name) => expand(name, vars.get(name), "")
      })
      result = """\$((?:\w|\|)+)\(([^)]+)\)""".r("name", "expression").replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name, exp) => expandMultiple(name, vars, exp)
      })
      result = """\$(\w+)""".r.replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name) => expand(name, vars.get(name), "")
      })
      result
    }

    def expandMultiple(name: String, vars: MetadataMap, format: String): String = {
      name.split("\\|").find { vars.contains(_) }.map(x => expand(x, vars.get(x), format)).headOption.getOrElse(format)
    }

    def expand(name: String, data: Option[MetadataProvider], format: String): String = {
      if (data.isEmpty)
        format
      else
        data.get.apply(format).getOrElse(format)
    }
    def extractExifWithExifTool(prefix: String, file: File) =
      Try {

        def split(text: String): Pair[String, String] = {
          val all = text.splitAt(text.indexOf(":"))
          Pair(all._1.trim.replaceAll("[/ ]", ""), all._2.drop(1).trim)
        }
        //println("Coulnd't get exif info from " + file)
        import scala.sys.process._
        import scala.sys.process.ProcessIO
        val pb = Process(s"""exiftool "${file.getAbsolutePath}"""")
        var map = Map[String, String]()
        val pio = new ProcessIO(_ => (),
          stdout => scala.io.Source.fromInputStream(stdout)
            .getLines.foreach { x =>
              //println(s"found $x")
              map += split(x)
            },
          _ => ())
        val a = pb.run(pio)
        val blockTillExits = a.exitValue
        if (blockTillExits == 0) {
          //println(map)
          //"exiftool".!
          //println(map mkString "\n")
          val result = map.toMap.map { x =>
            //println(x)
            (prefix + x._1, formatted(x._2)_)
          }
          result
        } else {
          throw new RuntimeException(s"Coulnd't get exif info from " + file + ". Got $blockTillExits from exiftool.")
        }
      }

    def computeMetadata(file: File): MetadataMap = {
      def extractExif2(prefix: String, file: File) = {
        val exifTry = Try { extractExifAsMap(file).map(x => (prefix + x._1, x._2)) }
        if (exifTry.isFailure) {
          extractExifWithExifTool(prefix, file)
        } else {
          exifTry
        }
      }

      val exifPrefix = "exif"
      //exif metadata
      val exif: MetadataMap = extractExif2(exifPrefix, file).orElse(
        if (Locations.file(file).extension.equalsIgnoreCase("avi"))
          //maybe exif@thm ?
          extractExif2(exifPrefix, Locations.file(file).withExtension(_ => "THM").toFile)
        else
          Failure(new RuntimeException())).getOrElse(Map())
      //exif for avi in pair thm file?
      //exif metadata

      //file properties
      import java.nio.file._
      import java.nio.file.attribute._
      val atts = Files.readAttributes(file.toPath, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
      import org.joda.time.DateTime
      val fileAttributes: MetadataMap = Map(
        tagFileModificationDateTime -> formatted(new DateTime(atts.lastModifiedTime.toString).toString(exifDateTimeFormatter))_,
        tagFileCreated -> formatted(new DateTime(atts.creationTime.toString).toString(exifDateTimeFormatter))_)
      val all = exif ++ fileAttributes
      //file system data
      val location = Locations.file(file)
      val format = extractFormat(file, all)
      val fs: MetadataMap = Map(tagFileExtension -> formatted(location.extension)_,
        tagCompRemaining -> formatted(cleanFormat(format))_,
        tagCompDetectedFormat -> formatted(format)_)
      val result = fs ++ all
      TreeMap(result.toSeq: _*)
    }

    def remainingFormat(file: File) = {
      var format = extractFormat(file, RichExif.computeMetadata(file))
    }
    def remainingFormat(file: File, metadata: MetadataMap) = {
      var format = extractFormat(file, metadata)
      cleanFormat(format)
    }
    def cleanFormat(format: String) = {
      //this assumes that usually after $ variable a separator might come
      var result = format.replaceAll("""\$\{[^}]+\}[._-]?""", "")
      result = result.replaceAll("^[._-]+", "")
      result = result.replaceAll("[._-]+$", "")
      result
    }
    type TransformValue = (Any) => String
    def formatted(value: Any)(format: String): MetadataResult = {
      if (format.isEmpty)
        Some(value.toString)
      else {
        //assume is date
        extractDate(value.toString).map { date =>
          val format2 = fromIrfanViewToJodaDateTime(format)
          date.toString(format2.replaceAll("%", ""))
        }.toOption
        //date
      }
    }
    def fromIrfanViewToJodaDateTime(format: String) = {
      //%Y-%m-%d--%H-%M-%S
      var result = format
      result = result.replaceAllLiterally("%Y", "yyyy")
      result = result.replaceAllLiterally("%m", "MM")
      result = result.replaceAllLiterally("%d", "dd")
      result = result.replaceAllLiterally("%H", "HH")
      result = result.replaceAllLiterally("%M", "mm")
      result = result.replaceAllLiterally("%S", "ss")
      result
    }
    def extractDate(text: String): Try[org.joda.time.DateTime] = {
      import org.joda.time.DateTime
      import org.joda.time.format.DateTimeFormat
      Try { DateTime.parse(text.trim, exifDateTimeFormatter) }.
        orElse(Try { DateTime.parse(text.trim, exifDateTimeFormatter2) }).
        orElse(Try { DateTime.parse(text.trim, exifDateTimeFormatter3) })
    }

    import com.thenewmotion.time.Imports._
    import org.apache.sanselan.common.{ IImageMetadata, RationalNumber }
    import org.apache.sanselan.formats.jpeg.JpegImageMetadata
    import org.apache.sanselan.formats.tiff.{ TiffField, TiffImageMetadata }
    import org.apache.sanselan.formats.tiff.constants.{ ExifTagConstants, GPSTagConstants, TagInfo, TiffConstants, TiffTagConstants }

    def formatIrfanView(fileName: String, pattern: String): String = {
      formatIrfanView(new File(fileName), pattern)
    }
    def formatIrfanView(fileName: File, pattern: String): String = {
      implicit val metadata = extractExif(fileName)
      val date = extractAsDate(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)
      var result = pattern
      val file = util.io.Locations.file(fileName)
      result = result.replaceAllLiterally("$F", file.name)
      //result = result.replaceAll("""\$E36867\(([^)]*)\)""","\\$1")
      result = result.replaceAll("""(\$E36867\([^)]*)%Y([^)]*\))""", "$1" + date.toString("yyyy") + "$2")
      result = result.replaceAll("""(\$E36867\([^)]*)%m([^)]*\))""", "$1" + date.toString("MM") + "$2")
      result = result.replaceAll("""(\$E36867\([^)]*)%d([^)]*\))""", "$1" + date.toString("dd") + "$2")
      result = result.replaceAll("""(\$E36867\([^)]*)%H([^)]*\))""", "$1" + date.toString("HH") + "$2")
      result = result.replaceAll("""(\$E36867\([^)]*)%M([^)]*\))""", "$1" + date.toString("mm") + "$2")
      result = result.replaceAll("""(\$E36867\([^)]*)%S([^)]*\))""", "$1" + date.toString("ss") + "$2")
      result = result.replaceAll("""(\$E36867\()([^)]+)\)""", "$2")
      result
    }
    def extractExif(fileName: File) = {
      import util.io.Locations
      val file = Locations.file(fileName)
      import org.apache.sanselan.Sanselan
      val metadata = try {
        Sanselan.getMetadata(file.toFile)
      } catch {
        case e: Throwable => throw new RuntimeException("Can't parse exif for " + file, e)
      }
      metadata
    }
    def extractAsDate(tag: TagInfo)(implicit metadata: org.apache.sanselan.common.IImageMetadata) = {
      val data = metadata.asInstanceOf[JpegImageMetadata].findEXIFValue(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)
      val date = DateTime.parse(data.getValue.toString.trim, DateTimeFormat.forPattern("yyyy:MM:dd HH:mm:ss"))
      date
    }
    def toSimpleMap(metadata: MetadataMap): Map[String, String] =
      metadata.mapValues(_("").getOrElse(null)).filter(x => x._2 != null)
    def extractFormat(file: File, metadata: MetadataMap): String = {
      val baseName = Locations.file(file).baseName
      var result = baseName
      var message = List[String]()

      def check(date1: Option[DateTime], prefix: String): Boolean = {
        if (date1.isDefined) {
          result = extractDateFromString(baseName, date1.get, prefix)
          val mappings = countSubstring(result, prefix)
          if (6 != mappings) {
            message ::= s"$prefix = $date1 matched [$result] but have only [$mappings]. 6 are needed."
            false
          } else {
            true
          }
        } else {
          message ::= s"$prefix doesn't exist."
          false
        }
      }
      //implicit val metadata = extractExif(file)
      //val date = extractAsDate(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)
      //println(computeMetadata(metadata).mkString("\n"))
      //val date1 = metadata.get("E36867").flatMap { _.apply(dateFormat) }.map { x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat)) }
      val date1 = metadata.get("exifDateTimeOriginal").flatMap { _.apply(dateFormat) }.map { x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat)) }
      if (baseName.length >= "yyyyMMddHHmmss".length) {
        //val stream = Stream(
        //  Pair("exifDateTimeOriginal", metadata.get("exifDateTimeOriginal").flatMap { _.apply(dateFormat) }.map { x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat)) }))
        //stream.find(x => check(x._2, x._1))
        if (!check(date1, "exifDateTimeOriginal")) {
          if (!check(date1.map(_.plusSeconds(1)), "exifDateTimeOriginal+1s")) {
            if (!check(date1.map(_.plusSeconds(2)), "exifDateTimeOriginal+2s")) {
              if (!check(date1.map(_.plusSeconds(3)), "exifDateTimeOriginal+3s")) {
                val date3 = metadata.get("exifModifyDate").flatMap { _.apply(dateFormat) }.map { x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat)) }
                if (!check(date3, "exifModifyDate")) {
                  if (!check(date3.map(_.plusSeconds(1)), "exifModifyDate+1s")) {
                    //val oldResult = result
                    //result = baseName
                    val date2 = metadata.get(tagFileModificationDateTime).flatMap { _.apply(dateFormat) }.map { x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat)) }
                    if (!check(date2, tagFileModificationDateTime)) {
                      println(s"Couldn't find a pattern in [$baseName]: ${message.reverse.mkString("\n\t", "\n\t", "\n")}")
                      result = baseName
                    }
                    //                    result = extractDateFromString(result, date2, "attribute-")
                    //                    if (6 != countSubstring(result, "${attribute-")) {
                    //                      println("couldn't find enough elements in [" + baseName + "]. Found [" + result + "] using $tagFileModificationDateTime [" + date2 + "] neither in " + message)
                    //                      result = baseName
                    //                    }
                  }
                }
              }
            }
          }
        }
      } else {
        println(s"Couldn't find a date pattern in [$baseName] is too short. Should have at least 14 characters to match something like yyyyMMddHHmmss.")
      }
      result
    }
    def countSubstring(str: String, substr: String) = Pattern.quote(substr).r.findAllMatchIn(str).length
    def extractDateFromString(text: String, date: DateTime, prefix: String): String = {
      var result = text
      result = replaceFirstLiterally(result, date.toString("yyyy"), "${" + prefix + "yyyy}")
      result = replaceFirstLiterally(result, date.toString("MM"), "${" + prefix + "MM}")
      result = replaceFirstLiterally(result, date.toString("dd"), "${" + prefix + "dd}")
      result = replaceFirstLiterally(result, date.toString("HH"), "${" + prefix + "HH}")
      result = replaceFirstLiterally(result, date.toString("mm"), "${" + prefix + "mm}")
      result = replaceFirstLiterally(result, date.toString("ss"), "${" + prefix + "ss}")
      result
    }
    def replaceFirstLiterally(text: String, literal: String, replacement: String): String = {
      val arg1 = java.util.regex.Pattern.quote(literal)
      val arg2 = java.util.regex.Matcher.quoteReplacement(replacement)
      text.replaceFirst(arg1, arg2)
    }
    def extractExifAsMap(file: File): MetadataMap = {
      val metadata = extractExif(file)
      extractExifAsMap(metadata)
    }
    def extractExifAsMap(metadata: org.apache.sanselan.common.IImageMetadata, extractKeyword: Boolean = true): MetadataMap = {
      import scala.collection.JavaConversions._
      var map: MetadataMap = Map()
      metadata.getItems().foreach {
        case item: TiffImageMetadata.Item =>
          if (extractKeyword) {
            val value = formatted(item.getTiffField.getValue)_
            map += item.getKeyword -> value
            map += item.getKeyword.replaceAll("\\s", "") -> value
            map += ("E" + item.getTiffField.tagInfo.tag) -> value
            val hex = ("0x" + Integer.toHexString(item.getTiffField.tagInfo.tag))
            map += ("E" + hex) -> value
          }
      }
      map
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