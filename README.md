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