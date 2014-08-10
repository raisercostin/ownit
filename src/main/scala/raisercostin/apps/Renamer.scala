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
import org.apache.commons.io.filefilter.RegexFileFilter

object Renamer {
  def main(args: Array[String]) = {
    if (args.length != 2) {
      println(s"""You must give two parameters and you gave ${args.toList.mkString("\n")}. The folder (that will NEVER be changed) with your media(pics,movies) files and the folder where you want to get a proposal of new names based on EXIF information.""")
    } else {
      ownPics(args(0), args(1))
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

  def ownPics(from: String, to: String, filter: Option[String] = None) = try {
    import RichExif._
    //main2(if(args.isEmpty) """d:\personal\photos\_desene\""" else args(0))
    //main2(""".\photo04.jpg""")
    //println(RichExif.formatIrfanView(""".\photo04.jpg""", "$E36867(%Y-%m-%d--%H-%M-%S)--$F"))
    //    main2(""".\20140617_191316_Bulevardul Mircea Vodă.jpg""")
    //println(RichExif.formatIrfanView(""".\20140617_191316_Bulevardul Mircea Vodă.jpg""", "$E36867(%Y-%m-%d--%H-%M-%S)--$F"))
    import util.io.Locations

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
    println(placeLocation.traverse.filter {
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
            val newName = RichExif.format(metadata, "$exifE36867|exifModifyDate|exifDateTimeOriginal(%Y-%m-%d--%H-%M-%S)---$exifFileNumber---$compRemaining.$fileExtension").replaceAll("[-]+[.]", ".")
            val ANSI_BACK = "" //"\u001B[1F";
            println(ANSI_BACK + "rename  " + file + " to " +
              newName + "\t\tdetectedFormat:" + metadata.get(tagCompDetectedFormat).map(_.apply("")).getOrElse(""))
            newName
          }
        val extensionsWithExif = Set("jpg", "jpeg", "gif", "mp4", "avi", "png", "bmp")
        val badChange = newName.isSuccess && newName.get.contains("%H-%M-%S") && extensionsWithExif.contains(src.extension.toLowerCase)
        val nameChanged = newName.isSuccess && !newName.get.contains("%H-%M-%S")
        val dest = Locations.file(if (badChange) placeBadFiles else placeGoodFiles)
        val baseName = if (nameChanged) newName.get else src.name
        dest.child(src.relativeTo(placeLocation)).withName(_ => baseName).mkdirOnParentIfNecessary.copyFromAsHardLink(src)
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
  object RichExif extends AutoCloseable {
    import com.thebuzzmedia.exiftool._
    lazy val tool = ExifToolService.Factory.create(Feature.STAY_OPEN, Feature.WINDOWS)
    def close = {
      tool.shutdown()
    }

    type MetadataResult = Try[String]
    type MetadataProvider = (String) => MetadataResult
    type MetadataMap = Map[String, MetadataProvider]
    def formatted(value: Any)(format: String): MetadataResult =
      //formatted2(value)(format).toOption
      formatted2(value)(format)

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
        case Regex.Groups(name) => expand(name, vars.get(name), "").getOrElse("")
      })
      result = """\$((?:\w|\|)+)\(([^)]+)\)""".r("name", "expression").replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name, exp) => expandMultiple(name, vars, exp).getOrElse(exp)
      })
      result = """\$(\w+)""".r.replaceAllIn(result, (_: scala.util.matching.Regex.Match) match {
        case Regex.Groups(name) => expand(name, vars.get(name), "").getOrElse("")
      })
      result
    }

    def expandMultiple(name: String, vars: MetadataMap, format: String): Try[String] = {
      val all = name.split("\\|")
      //println(s"""search in ${all mkString "\n"}""")
      name.split("\\|"). //filter{x=> vars.contains(x)}.
        map(x => expand(x, vars.get(x), format)).find(_.isSuccess).headOption.getOrElse(Failure(new RuntimeException("Couldn't find format")))
    }

    def expand(name: String, data: Option[MetadataProvider], format: String): Try[String] = {
      if (data.isEmpty) {
        //println("Couldn't find " + name + " found +" + data)
        Failure(new RuntimeException("Couldn't find " + name + " found +" + data))
      } else {
        val a = data.get.apply(format)
        //println(s"For $name found $a")
        //a.getOrElse(format)
        a
      }
    }
    def extractExifWithExifTool(prefix: String, file: File): Try[MetadataMap] =
      Try { extractExifUsingBuzzMedia(prefix, file) }

    def extractExifWithExifToolOld(prefix: String, file: File): Try[MetadataMap] =
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
        def extractExif2(prefix: String, file: File): Try[MetadataMap] = {
          val exifTry = Try { extractExifAsMap(file).map(x => (prefix + x._1, x._2)) }
          if (exifTry.isFailure) {
            extractExifWithExifTool(prefix, file)
          } else {
            exifTry
          }
        }

      //exif metadata
      val exifPrefix = "exif"
      val thmExifPrefix = "exif"
      val exifPrefix2 = "exif"
      val thmExifPrefix2 = "exif"
      val exifFomFile: MetadataMap = extractExif2(exifPrefix, file).getOrElse(Map())
      val exifFomFileWithExifTool: MetadataMap = extractExifUsingBuzzMedia(exifPrefix2, file)
      val pairThm = if (Locations.file(file).extension.equalsIgnoreCase("avi"))
        Some(Locations.file(file).withExtension(_ => "THM").toFile)
      else
        None
      val exifFromAssociatedThm: MetadataMap = pairThm.flatMap(file => extractExif2(thmExifPrefix, file).toOption).getOrElse(Map())
      val exifFromAssociatedThmUsingExifTool: MetadataMap = pairThm.map(file => extractExifUsingBuzzMedia(thmExifPrefix2, file)).getOrElse(Map())
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
      val all = exifFomFile ++ exifFomFileWithExifTool ++ exifFromAssociatedThm ++ exifFromAssociatedThmUsingExifTool ++ fileAttributes
      //println(toSimpleMap(all) mkString "\n")
      //file system data
      val location = Locations.file(file)
      val format = extractFormat(file, all)
      val fs: MetadataMap = Map(tagFileExtension -> formatted(location.extension)_,
        tagCompRemaining -> formatted(cleanFormat(format))_,
        tagCompDetectedFormat -> formatted(format)_)
      val result = fs ++ all
      TreeMap(result.toSeq: _*)
    }
    def extractExifUsingBuzzMedia(prefix: String, file: File): MetadataMap = {
      import scala.collection.JavaConversions._

      val valueMap = tool.getImageMeta(file).map(x => prefix + x._1.getKey() -> formatted(x._2)_)
      //println(valueMap mkString "\n")
      Map() ++ valueMap
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

    def formatted2(value: Any)(format: String): Try[String] = {
      if (format.isEmpty)
        if (value == null)
          Success("")
        else
          Success(value.toString)
      else {
        //assume is date
        extractDate(value).map { date =>
          val format2 = fromIrfanViewToJodaDateTime(format)
          //println(format2)
          date.toString(format2.replaceAll("%", ""))
        }
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
    def extractDate(value: Any): Try[org.joda.time.DateTime] = {
      import org.joda.time.DateTime
      import org.joda.time.format.DateTimeFormat
      import java.util.GregorianCalendar
      import java.util.Date
      value match {
        case text: String =>
          Try { DateTime.parse(text.trim, exifDateTimeFormatter) }.
            orElse(Try { DateTime.parse(text.trim, exifDateTimeFormatter2) }).
            orElse(Try { DateTime.parse(text.trim, exifDateTimeFormatter3) })
        case date: Date =>
          Try { new DateTime(date) }
        case date: GregorianCalendar =>
          Try { new DateTime(date) }
      }
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
      metadata.mapValues(_("").getOrElse(null)).filter(x => x._2 != null).mapValues(_.toString)
    def extractFormat(file: File, metadata: MetadataMap): String = {
      val baseName = Locations.file(file).baseName
      var result = baseName
      var message = List[String]()

        def check(date1: Try[DateTime], prefix: String): Boolean = {
          if (date1.isSuccess) {
            result = extractDateFromString(baseName, date1.get, prefix)
            val mappings = countSubstring(result, prefix)
            if (6 != mappings) {
              //actual good prefix is this
              message ::= s"$prefix = $date1 matched [$result] but have only [$mappings]. 6 are needed."
              false
            } else {
              true
            }
          } else {
            message ::= s"$prefix doesn't exist: " + date1.failed.get.getMessage
            false
          }
        }

        def extractDateTime(id: String) = metadata.get(id).fold[Try[String]] { Failure(new RuntimeException("Not found")) } { _.apply(dateFormat) }.map { x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat)) }

      //$exifE36867|exifModifyDate|exifDateTimeOriginal
      //implicit val metadata = extractExif(file)
      //val date = extractAsDate(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL)
      //println(computeMetadata(metadata).mkString("\n"))
      //val date1 = metadata.get("E36867").flatMap { _.apply(dateFormat) }.map { x => DateTime.parse(x, DateTimeFormat.forPattern(dateFormat)) }
      if (baseName.length >= "yyyyMMddHHmmss".length) {
        lazy val date1: Try[DateTime] = extractDateTime("exifDateTimeOriginal")
        lazy val date3: Try[DateTime] = extractDateTime("exifModifyDate")
        lazy val date2: Try[DateTime] = extractDateTime(tagFileModificationDateTime)
        lazy val date4: Try[DateTime] = extractDateTime("exifE36867")
        val stream = Stream(
          Pair("exifDateTimeOriginal", date1), Pair("exifDateTimeOriginal+1s", date1.map(_.plusSeconds(1))), Pair("exifDateTimeOriginal+2s", date1.map(_.plusSeconds(2))), Pair("exifDateTimeOriginal+3s", date1.map(_.plusSeconds(3))), Pair("exifModifyDate", date3), Pair("exifModifyDate+1s", date3.map(_.plusSeconds(1))), Pair("exifModifyDate+2s", date3.map(_.plusSeconds(2))), Pair("exifModifyDate+3s", date3.map(_.plusSeconds(3))), Pair("exifE36867", date4), Pair("exifE36867+1s", date4.map(_.plusSeconds(1))), Pair("exifE36867+2s", date4.map(_.plusSeconds(2))), Pair("exifE36867+3s", date4.map(_.plusSeconds(3))), Pair("tagFileModificationDateTime", date2), Pair("tagFileModificationDateTime+1s", date2.map(_.plusSeconds(1))), Pair("tagFileModificationDateTime+2s", date2.map(_.plusSeconds(2))), Pair("tagFileModificationDateTime+3s", date2.map(_.plusSeconds(3))))
        val a = stream.find(x => check(x._2, x._1))
        //println("a=" + a)
        if (a.isEmpty) {
          println(s"Couldn't find a pattern in [$baseName]: ${message.reverse.mkString("\n\t", "\n\t", "\n")}")
          result = baseName
        }
      } else {
        println(s"Couldn't find a date pattern in [$baseName] is too short. Should have at least 14 characters to match something like yyyyMMddHHmmss.")
      }
      result
    }
    def countSubstring(str: String, substr: String) = Pattern.quote(substr).r.findAllMatchIn(str).length
    def extractDateFromString(text: String, date: DateTime, prefix2: String, suffix: String = "+"): String = {
      var result = text
      val prefix = "$$$$"
      result = replaceFirstLiterally(result, date.toString("yyyy"), "${" + prefix + suffix + "yyyy}")
      result = replaceFirstLiterally(result, date.toString("MM"), "${" + prefix + suffix + "MM}")
      result = replaceFirstLiterally(result, date.toString("dd"), "${" + prefix + suffix + "dd}")
      result = replaceFirstLiterally(result, date.toString("HH"), "${" + prefix + suffix + "HH}")
      result = replaceFirstLiterally(result, date.toString("mm"), "${" + prefix + suffix + "mm}")
      result = replaceFirstLiterally(result, date.toString("ss"), "${" + prefix + suffix + "ss}")
      result = replaceAllLiterally(result, "${" + prefix, "${" + prefix2)
      result
    }
    def replaceFirstLiterally(text: String, literal: String, replacement: String): String = {
      val arg1 = java.util.regex.Pattern.quote(literal)
      val arg2 = java.util.regex.Matcher.quoteReplacement(replacement)
      text.replaceFirst(arg1, arg2)
    }
    def replaceAllLiterally(text: String, literal: String, replacement: String): String = {
      val arg1 = java.util.regex.Pattern.quote(literal)
      val arg2 = java.util.regex.Matcher.quoteReplacement(replacement)
      text.replaceAll(arg1, arg2)
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