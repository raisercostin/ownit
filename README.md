ownit
=====

Content Organizer Assistant – automatic heuristic metadata extractor of Exif(images), ID3 tag(mp3)

Rename pattern:
$<tag1>|<tag2>|...|<tagN>(<microformat>)

- tagX - a metadata tag. The tags are tried till one is found from left to right.
    - exif<CamelCaseExifTag> - an exif tag has the exif prefix
	- file<FileTag> - tags extracted using the file system
	     - fileCreated
		 - fileModified 
	- comp<ComputedTag> - tags that are computed after all the other tags are created
	    - compDetectedFormat - tag that attempts to reconstruct a format out of current name and the other tags
		- compRemaining - tag with the file name data remaining after all the recognized data from detectedFormat was extracted
- microformat - %yyyy, %MM, %dd, %HH, %mm, %ss - for date time fields

= Resources =
- http://www.thebuzzmedia.com/software/exiftool-enhanced-java-integration-for-exiftool/

= Execute = 
- create executable
sbt oneJar
- propose new organized folder without modifying source folder
ownit.bat d:\personal\photos2\ d:\personal\photos2-george
- display exif info for a file
ownit.bat d:\personal\photos2-george-bad\Camera\20140906_000621.jpg