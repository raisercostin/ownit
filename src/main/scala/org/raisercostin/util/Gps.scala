package org.raisercostin.util.gps

import org.raisercostin.jedi._

case class Distance(meters: Double) {
  def toInternational =
    if (meters >= 1.0)
      f"$meters%.1f km"
    else
      f"${meters / 1000}%.0f m"
}
object Gps {
  //http://download.geonames.org/export/dump/cities1000.zip
  lazy val locations = fromFile(Locations.classpath("cities1000.zip").unzip)
  def fromFile(src: NavigableInputLocation): Seq[Gps] = {
    //    0         1            2          3                       4          5    6   7   8       9                10           11         12            13
    //3039154	El Tarter	El Tarter	Ehl Tarter,Эл Тартер	42.57952	1.65362	P	PPL	AD		02				1052		1721	Europe/Andorra	2012-11-03
    src.child("cities1000.txt").readLines.map { line =>
      val fields = line.split("\t")
      Gps(fields(4), "N", fields(5), "E", "12", "0", Some(fields(1)))
    }.toSeq
  }
  def custom = Seq(
    Gps("44.860046", "N", "24.867838", "E", "13.0", "0", Some("pitesti")),
    Gps("44.4378258", "N", "26.0946376", "E", "12", "0", Some("bucuresti")),
    Gps("50.854975", "N", "4.3753899", "E", "12", "0", Some("brussels")))
  def apply(GPSLatitude: String, GPSLongitude: String) =
    new Gps(GPSLatitude, if (GPSLatitude.toDouble >= 0) "N" else "S", GPSLongitude, if (GPSLongitude.toDouble >= 0) "E" else "W", "0", "0", None)
}
//https://www.google.com/maps/place/@44.85597,24.8735028,13z
//https://www.google.com/maps/place/Pite%C8%99ti,+Romania/@44.85597,24.8735028,13z
//https://www.google.com/maps/place/44%C2%B051'21.5%22N+24%C2%B052'24.6%22E/@44.85597,24.8735028,17z/data=!3m1!4b1!4m2!3m1!1s0x0:0x0
//44.860046, 24.867838
//44°51'21.5"N 24°52'24.6"E
case class Gps(GPSLatitude: String, GPSLatitudeRef: String, GPSLongitude: String, GPSLongitudeRef: String, GPSAltitude: String, GPSAltitudeRef: String, name: Option[String] = None) {
  def latitude = GPSLatitude.toDouble
  def longitude = GPSLongitude.toDouble
  def distanceTo(to: Gps) = distance(latitude, longitude, to.latitude, to.longitude)
  private def distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double) = {
    var R = 6371; // km
    var dLat = toRad(lat2 - lat1);
    var dLon = toRad(lon2 - lon1);
    var lat1R = toRad(lat1);
    var lat2R = toRad(lat2);
    var a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.sin(dLon / 2) * Math.sin(dLon / 2) * Math.cos(lat1R) * Math.cos(lat2R);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    var d = R * c;
    Distance(d * 1000)
  }

  private def toRad(value: Double): Double = value * Math.PI / 180

  def mapHref = s"https://www.google.com/maps/@${GPSLatitude},${GPSLongitude},14z"
  //
  //  def distance(position: Position): Option[String] = current map { x => distance(position, x) }
  def closestLocation: Gps = {
    val b = Gps.locations.toIterator.filter(location => near(location.latitude, 10))
    val a = b.minBy(place => distanceTo(place).meters) //map(place => (place, distanceTo(place)))
    //println(a)
    //a.minBy(_._2.meters)._1
    a
  }
  //Each degree of latitude is approximately 69 miles (111 kilometers) apart.
  def near(newLatitude: Double, delta: Double) = (latitude - newLatitude).abs * 111 <= delta
}
