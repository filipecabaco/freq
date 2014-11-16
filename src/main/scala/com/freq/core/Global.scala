import scala.collection.immutable.TreeMap
import scala.concurrent._
import scala.io
import scala.util.matching.Regex
import scala.util.{Success, Failure}

import org.json4s._
import org.json4s.native.JsonMethods._

import ExecutionContext.Implicits.global
import java.io.FileNotFoundException


case class Destination(name:String,id:Int)

object Global extends App{

  val ids = args.toList
  println(s"Started with ids $ids" )
  execute

  def execute = {
    val futures = ids.map{d=>Future(Tools.getDestInfo(d))}
    futures.map( _ onComplete{
      case Success(dest) => showWordCount(dest,Tools.getAllHotelDescriptions(dest).mkString)
      case Failure (t) => println("Error thrown - %s".format(t.getClass().getSimpleName()))
      })
  }

  private def showWordCount(dest:Destination,text : String){
    val words = new Regex("[a-zA-Z]+","g") findAllIn text
    var res = collection.mutable.Map[String,Int]().withDefaultValue(0)
    for(w<- words) {
      res.update(w,(res(w)+1))
    }
    println("Destination %s with Id %d:",dest.name,dest.id)
    res.toSeq.sortBy(_._2).reverse.foreach(t=>println(t._1 + " :"+t._2))
  }
}

object Tools{
  //URL to get information of destionation
  val destinationInfoUrl = "http://concierge.top10.com/v1/destinations/%s"
  val hotelInfoUrl = "http://concierge.top10.com/v1/destinations/%s/hotels?hotel_attrs=description&count=20"
  def getDestInfo(destId:String) : Destination = {
    val obj = getJsonFromUrl(destinationInfoUrl.format(destId))

    //Extract name from JSON
    val name = for {
      JObject(o) <- obj
      JField("id",JString(id)) <- o
      if id == destId
      JField("name",JString(n)) <- o
    } yield n

    new Destination(name.mkString,destId.toInt)
  }

  def getAllHotelDescriptions(dest : Destination) : List[String] = {
    val obj = getJsonFromUrl(hotelInfoUrl.format(dest.id))
    val descritpions = for {
      JObject(o) <- obj
      JField("text",JString(d)) <- o
    } yield d
    descritpions
  }

  private def getJsonFromUrl(url:String) : JValue = {
    parse(scala.io.Source.fromURL(url).mkString)
  }
}


