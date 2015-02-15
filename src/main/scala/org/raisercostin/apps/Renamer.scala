package org.raisercostin.apps
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
import org.joda.time.DateTime
import com.thebuzzmedia.exiftool.ExifToolNew3
import scala.collection.immutable.StringOps
import org.raisercostin.util.io.NavigableLocation
import org.raisercostin.util.io.RelativeLocation

object InternalRenamer {
  def main(args: Array[String]) = {
    Renamer.main(Array("""d:\personal\photos-tofix\2013"""))
    //Renamer.main(Array("""d:\personal\photos-tofix\2013-temp"""))
  }
}
object Renamer {
  def main(args: Array[String]) = {
    //test
    main2(args)
    //ownPics("""d:\personal\work\conta\brainlight&pfa\440_0111\""","""proposed1""")
    //ownPics("""d:\personal\photos-tofix\photos-old-todelete\""", """-proposed2""")
    //ownPics("""d:\personal\photos-tofix\2013-proposed1-bad\""","""-proposed3""")
    //System.exit(0)
  }
  def main2(args: Array[String]) = {
    args match {
      case Array(from: String, to: String, "-debug") =>
        organize(Some(from), to, true)
      case Array(from: String, "-debug") =>
        organize(Some(from), "-proposed", true)
      case Array(from: String, "-gui") =>
        RenamerGUI.main(args)
      case Array(from: String, to: String) =>
        organize(Some(from), to)
      case Array(from: String) =>
        organize(Some(from))
      case Array() =>
        println(s"""You must give two parameters and you gave ${args.size}: ${args.toList.mkString("\n")}. \nThe folder (that will NEVER be changed) with your media(pics,movies) files and the folder where you want to get a proposal of new names based on EXIF information.""")
        RenamerGUI.main(args)
      case _ =>
        organize(None, args = args)
    }
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
    toDevices.mkdirOnParentIfNecessary
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
  def organize(fromPath: Option[String], toRelativeOrAbsolute: String = "-proposed", debug: Boolean = false, filter: Option[String] = None, args: Array[String] = Array()): Unit =
    fromPath match {
      case Some(fromPath2) => organize2(fromPath2, toRelativeOrAbsolute, debug, filter)
      case _ =>
        println(s"""You must give two parameters and you gave ${args.size}: ${args.toList.mkString("\n")}. \nThe folder (that will NEVER be changed) with your media(pics,movies) files and the folder where you want to get a proposal of new names based on EXIF information.""")
    }

  def organize2(fromPath: String, toRelativeOrAbsolute: String = "-proposed", debug: Boolean = false, filter: Option[String] = None) = try {
    //if(debug)
    println(s"organize [$fromPath] -> [$toRelativeOrAbsolute]")
    val cities = Locations.classpath("cities1000.zip")
    println(cities)
    //if(debug) {} 
    val oldValue = Option(java.lang.System.getProperty(ExifToolNew3.ENV_EXIF_TOOL_PATH)).filter(_.nonEmpty)
    if (!oldValue.isDefined) {
      val srcExifTool = Locations.classpath("exiftool2.exe")
      val exifTool = Locations.temp.child("exiftool/exiftool2.exe").asFile.nonExisting(_.copyFrom(srcExifTool)).absolute
      java.lang.System.setProperty(ExifToolNew3.ENV_EXIF_TOOL_PATH, exifTool)
      println(s"Use exiftool from $exifTool copied from $srcExifTool. Environment variable [${ExifToolNew3.ENV_EXIF_TOOL_PATH}] is not defined.")
    } else {
      println(s"Use exiftool defined by [${ExifToolNew3.ENV_EXIF_TOOL_PATH}] as [${oldValue.get}]")
    }
    val from = Locations.file(fromPath)
    val to = Locations.file(toRelativeOrAbsolute, from).renamedIfExists
    println("organize from " + from.absolute + " to " + to.absolute)
    val placeBadFiles = to.child("strangeMedia")
    val placeGoodFiles = to
    implicit val allDevices = DevicesDao(Set(), placeGoodFiles.child("devices.txt").renamedIfExists)
    from.traverse.filter {
      case (file1, x) =>
        val file = file1.toFile
        filter.isEmpty || fileWildcard(filter.get, file.getAbsolutePath)
    }.map {
      case (file1, x) =>
        val file = file1.toFile
        val src = Locations.file(file)
        val res = process(src, from, placeBadFiles, placeGoodFiles)(debug).recover {
          case e =>
            if (debug)
              e.printStackTrace
            else
              println(s"Error processing $src: ${e.getMessage()}")
        }
    }
    /*
     .filter(_.isFailure).map {
      case Failure(f) => 
        if(debug)
          dump(f)
        else
          println(f.getMessage)
    }
    */
    println("done.")
  }

  val standardNameXXXX = dateAnalyser + "$exifFileNumberMajor|(---%%|---XXX)$exifFileNumberMinor|XXXX(-IMG_%%)$compClosestLocation|XXX(---at-%%)$compRemaining|(--%%|)$fileExtension(.%%)"
  val standardName = dateAnalyserNoXXXX + "$exifFileNumberMajor|(---%%|)$exifFileNumberMinor|(-IMG_%%|)$compClosestLocation|(---at-%%|)$compRemaining|(--%%|)$fileExtension(.%%)"

  val allRenamers = Seq(
    Formatter("byYearMonth", tags => tags.dateGroup2 + "|(yyyy-MM-MMMM" + Locations.SEP + "|)" + standardName, AnalysedFolder),
    //
    //Formatter("flat-XXXX", _ => standardNameXXXX, false),
    Formatter("flat", _ => standardName, NoFolder),
    Formatter("standard", _ => standardName, KeepFolder),
    Formatter("standardSimplified", _ => standardName, AnalysedFolder) //Formatter("standard-XXXX", _ => standardNameXXXX, true),
    //Formatter("byYear", tags => tags.dateGroup + "|(yyyy|XXXX)" + Locations.FILE_SEPARATOR + standardName, AnalysedFolder),
    //Formatter("byYear-XXXX", tags => tags.dateGroup + "|(yyyy|XXXX)" + Locations.FILE_SEPARATOR + standardNameXXXX, false),
    //Formatter("byYearMonth", tags => tags.dateGroup + "|(yyyy-MM-MMMM|XXXX)" + Locations.SEP + standardName, AnalysedFolder) //
    //Formatter("byYearAndYearMonth", tags => tags.dateGroup + "|(yyyy|XXXX)-" + tags.dateGroup + "|(MM-MMMM|XX)" + Locations.FILE_SEPARATOR + standardName, AnalysedFolder),
    //Formatter("byYearMonthFlat", tags => tags.dateGroup + "|(yyyy|XXXX)-" + tags.dateGroup + "|(MM-MMMM|XX)" + Locations.FILE_SEPARATOR + standardName, NoFolder) //Formatter("byYearMonth-XXXX", tags => tags.dateGroup + "(yyyy)|(XXXX)-" + tags.dateGroup + "(MM-MMMM)|(XX)" + Locations.FILE_SEPARATOR + standardNameXXXX, false),
    //Formatter("byCounter", _ => "$exifFileNumberMajor|(%%|)$exifFileNumberMinor|(-%%|)$compRemaining|(-%%|)$fileExtension(.%%)", false),
    //Formatter("byCounter-XXXX", _ => "$exifFileNumberMajor|(%%|XXX)$exifFileNumberMinor|(-%%|-XXXX)$compRemaining|(-%%|)$fileExtension(.%%)", false),
    //Formatter("byCounterKeepStructure", _ => "$exifFileNumberMajor|(%%|)$exifFileNumberMinor|(-%%|)$compRemaining|(-%%|)$fileExtension(.%%)", true),
    //Formatter("byCounterKeepStructure-XXXX", _ => "$exifFileNumberMajor|(%%|XXX)$exifFileNumberMinor|(-%%|-XXXX)$compRemaining|(-%%|)$fileExtension(.%%)", true)
    )

  def process(src: FileLocation, from: FileLocation, placeBadFiles: FileLocation, placeGoodFiles: FileLocation)(debug: Boolean)(implicit devices: DevicesDao) = {
    println("analyze " + src.absolute + " ...")
    val newName = Try {
      val tags = raw.loadExifTags(src)
      val maxDateTime = new DateTime(2015, 8, 1, 0, 0, 0)
      if (tags.localDateTime.map { _.toDateTime().isAfter(maxDateTime) }.getOrElse(false)) {
        throw new RuntimeException(s"The application cannot process files older than ${maxDateTime.toString("yyyy-MM-dd")}. You should update the application.")
      }
      devices.checkDeviceId(tags)
      println("\tdetected format\t" + tags.analyse(src.name).get)
      allRenamers.foreach { formatter =>
        val name = formatter.folder
        val path2 = src.extractPrefix(from)
        val newDestFile = formatter.proposal(placeBadFiles.withBaseName(_ + "-" + name), placeGoodFiles)(path2, tags).mkdirOnParentIfNecessary.renamedIfExists
        val ANSI_BACK = "" //"\u001B[1F";
        //println(ANSI_BACK + "\t"+name.padTo(30,' ')+"> smartcopy to\t" + newDestFile.absolute)
        println(ANSI_BACK + "\t" + name.padTo(30, ' ') + "-> \t" + newDestFile.extractPrefix(placeGoodFiles))
        src.copyAsHardLink(newDestFile)
      }
    }
    newName.recover {
      case e =>
        e.printStackTrace()
        placeBadFiles.child(src extractPrefix (from)).mkdirOnParentIfNecessary.inspect(x => println("bad file " + x + " with error " + e.getMessage())).copyFromAsHardLink(src)
        Failure(e)
    }
  }
  trait FolderStrategy
  object KeepFolder extends FolderStrategy
  object AnalysedFolder extends FolderStrategy
  object NoFolder extends FolderStrategy

  case class Formatter(folder: String, patternSupplier: (ExifTags) => String, keepStructure: FolderStrategy) {
    def remainingFolder(category: String, tags: ExifTags) = {
      val analysedCategory = tags.analyse(category, Seq("compClosestLocation")).getOrElse(category)
      val clean = tags.cleanFormat(analysedCategory).replaceAllLiterally(Locations.SEP_STANDARD, "--")
      val remaining = Option(clean).filter(_.length > 0)
      remaining
    }

    def proposal[T <: NavigableLocation](placeBadFiles: T, rootPlaceGoodFiles: T)(path2: RelativeLocation, tags: ExifTags): T = {
      val exifFileName = tags("exifFileName")
      require(exifFileName.isDefined, "Tags should contain key [exifFileName].")
      val fileExtension = tags("fileExtension").get
      require(exifFileName.get.endsWith(fileExtension), s"Tag exifFileName=[${exifFileName.get}] should end with the value of fileExtension=[$fileExtension].")
      //      val fileNameAnalysis = tags.analyse(path2.name)
      //      val pathAnalysis = tags.analyse(path2.parent.relativePath)
      //
      val imageOrVideo = tags.isImage || tags.isVideo
      val placeGoodFiles = rootPlaceGoodFiles.child(folder)
      if (imageOrVideo) {
        val pattern = patternSupplier(tags)
        val newName = tags.interpolate(pattern).get
        val category = path2.parent.standard(_.relativePath)
        val remaining = remainingFolder(category, tags)
        val basePath = Locations.relative(newName)
        val baseFolder = basePath.parent
        val baseName = basePath.name
        val place = keepStructure match {
          case KeepFolder =>
            placeGoodFiles.child(baseFolder).child(category)
          case AnalysedFolder =>
            if (imageOrVideo && baseFolder.nonEmpty)
              placeGoodFiles.child(baseFolder).withBaseName2(x => remaining.map { y => if (x.nonEmpty && y.nonEmpty) x + "--" + y else x + y })
            else
              placeGoodFiles.child(baseFolder).child(remaining)
          case NoFolder =>
            placeGoodFiles.child(baseFolder)
        }
        place.child(baseName)
      } else {
        keepStructure match {
          case KeepFolder =>
            placeGoodFiles.child(path2)
          case _ =>
            placeGoodFiles.child("unorganized").child(path2)
        }
      }
    }
  }
  //  def standardizeName(placeBadFiles: FileLocation, placeGoodFiles: FileLocation)(from: FileLocation, src: FileLocation, tags: ExifTags): FileLocation = {
  //    val newName = tags.interpolate(dateAnalyser + "---$exifFileNumberMajor|(%%|XXX)-IMG_$exifFileNumberMinor|(%%|XXXX)---at-$compClosestLocation|(%%|XXX)$compRemaining|(--%%|)$fileExtension(.%%)").get
  //    val imageOrVideo = tags.isImage || tags.isVideo
  //    val badName = newName.contains("XXXX-XX-XX--XX-XX-XX")
  //    val badChange = badName && imageOrVideo
  //    val nameChanged = !badName && imageOrVideo
  //    val dest = if (badChange) placeBadFiles else placeGoodFiles
  //    val baseName = if (nameChanged) newName else src.name
  //    dest.child(src.relativeTo(from)).withName(_ => baseName).mkdirOnParentIfNecessary.renamedIfExists
  //  }

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
}