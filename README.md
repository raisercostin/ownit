#Ownit

Content Organizer Assistant – automatic heuristic metadata extractor of Exif(images), ID3 tag(mp3)


# Status

[![Download](https://api.bintray.com/packages/raisercostin/maven/ownit/images/download.svg)](https://bintray.com/raisercostin/maven/ownit/_latestVersion)
[![Build Status](https://travis-ci.org/raisercostin/ownit.svg?branch=master)](https://travis-ci.org/raisercostin/ownit)
[![Codacy Badge](https://www.codacy.com/project/badge/fe1bb28a7735433d89a238ce6f6305c1)](https://www.codacy.com/app/raisercostin/ownit)
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
- propose new organized folder without modifying source folder
organize.bat d:\personal\photos2\ d:\personal\photos2-george
- display exif info for a file
organize.bat d:\personal\photos2-george-bad\Camera\20140906_000621.jpg


## How to use ##
 - Download one-jar files from http://dl.bintray.com/raisercostin/maven/org/raisercostin/ownit_2.10/0.8/ownit_2.10-0.8-one-jar.jar
 
## Development ##
 - to release

 ```
 sbt> oneJar
 sbt> release skip-tests
 ```
