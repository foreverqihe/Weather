package elf.qi

import java.io.File
import org.json4s._
import org.json4s.jackson.JsonMethods._

import java.io.InputStream
import java.time.LocalDateTime
import java.util.Date
import java.time.format.DateTimeFormatter

case class City(val name: String, val code: String, val meta: List[Meta])
case class Meta(val month: Int, val mean: Double, val range: Double, val conditions: Map[String, Double])

class Weather(val city: City, val time: LocalDateTime, val condition: String, val temperature: Double, val pressure: Double, val humidity: Int) {

}

object Main {
  def main(args: Array[String]) {

    implicit val formats = DefaultFormats
    val cities: List[City] = getListOfFiles(args(0)).map { f =>
      val content = io.Source.fromFile(f).getLines.mkString("\n")
      val json = parse(content)
      json.extract[City]
    }

    getWeatherData(cities, LocalDateTime.now).map { d => 
      List(d.city.code, d.time.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME), d.condition, d.temperature, d.pressure, d.humidity).mkString("|")
    }.foreach(println)
  }

  def getWeatherData(cities: List[City], time: LocalDateTime): List[Weather] = {
    cities.map { city =>
      val currentMeta = city.meta(time.getMonthValue - 1)

      val temperature = getTemperature(currentMeta, time)
      val weatherCondition = getWeatherCondition(currentMeta)
      val pressure = getAirPressure(weatherCondition, temperature, currentMeta)
      val humidity = getHumidity(weatherCondition, temperature, currentMeta)
      new Weather(city, time, weatherCondition, temperature, pressure, humidity)
    }
  }

  def getHumidity(cond: String, temperature: Double, meta: Meta): Int = {
    val guess = cond match {
      case "Cloudy" => 40
      case "Rain"   => 80
      case "Snow"   => 60
      case "Sunny"  => 30
      case _        => 50
    }
    val toOne = 100 - guess
    if (temperature > meta.mean) {
      guess + util.Random.nextInt(toOne / 3)
    } else {
      guess - util.Random.nextInt(guess / 3)
    }
  }

  def getAirPressure(cond: String, temperature: Double, meta: Meta): Double = {
    val guessPressure = cond match {
      case "Cloudy" => 1030.0 + util.Random.nextInt(10) + util.Random.nextDouble()
      case "Rain"   => 1015.0 + util.Random.nextInt(10) + util.Random.nextDouble()
      case "Snow"   => 1010.0 + util.Random.nextInt(10) + util.Random.nextDouble()
      case "Sunny"  => 1035.0 + util.Random.nextInt(10) + util.Random.nextDouble()
      case _        => 1013.0 + util.Random.nextDouble()
    }
    if (temperature > meta.mean)
      guessPressure - util.Random.nextInt(20)
    else
      guessPressure + util.Random.nextInt(20)

  }

  def getWeatherCondition(meta: Meta): String = {
    val dist = meta.conditions.map { case (k, v) => (k, v / 30.0) }
    val p = scala.util.Random.nextDouble
    val it = dist.iterator
    var accum = 0.0
    while (it.hasNext) {
      val (item, itemProb) = it.next
      accum += itemProb
      if (accum >= p)
        return item // return so that we don't have to search through the whole distribution
    }
    sys.error(f"this should never happen") // needed so it will compile
  }

  def getTemperature(meta: Meta, time: LocalDateTime): Double = {

    // assume temperature is a sin with 10 am is the medium temperature
    meta.range * Math.sin(((time.getHour - 10) / 12) * Math.PI) + meta.mean
  }

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).filter { f => f.getName.endsWith("json") }.toList
    } else {
      List[File]()
    }
  }
}