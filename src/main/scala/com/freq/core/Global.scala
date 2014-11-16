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
  val pttr = "[a-zA-Z]+"
  val output = "Destination %s with Id %d:"
  println(s"Started with ids $ids" )
  executeWithFuture
  /**
  * Executes the word search wrapped in a Future
  */
  def executeWithFuture = {
    val futures = ids.map{id=>Future(Tools.getDestInfo(id))}
    futures.map( _ onComplete{
      case Success(dest) => {
       val res = getOrderedWordCount(Tools.getAllHotelDescriptions(dest).mkString)
       if(res.length > 0){ showOutput(dest,res) }
      }
      case Failure (t) => println("Error thrown - %s".format(t.getClass().getSimpleName()))
    })
  }
  /**
   * Counts the word occurence for a given String, returning a sequence of tuples order by word count
   */
  private def getOrderedWordCount(text : String) : Seq[(String,Int)] = {
    val words = new Regex(pttr,"g") findAllIn text
    var res = collection.mutable.Map[String,Int]().withDefaultValue(0)

    for(w<- words) {
      res.update(w,(res(w)+1))
    }
    res.toSeq.sortBy(_._2).reverse.toSeq
  }

/**
 * Prints out the full output of one operation
 */
  private def showOutput(dest:Destination,seq:Seq[(String,Int)]){
    //To ensure the output is shown correctly
    this.synchronized {
      println(output.format(dest.name,dest.id))
      seq.foreach(t=>println(t._1 + " :"+t._2))
    }
  }
}
/**
 * Simple tools used in the main code
 */
object Tools{
  //URL to get information of destionation
  val destinationInfoUrl = "http://concierge.top10.com/v1/destinations/%s"
  val hotelInfoUrl = "http://concierge.top10.com/v1/destinations/%s/hotels?hotel_attrs=description&count=20"

  /**
   * Retrieves information of the Destination based on his ID
   */
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
  /**
   * Retrieves all the descriptions for the hotels in the given Destination
   */
   def getAllHotelDescriptions(dest : Destination) : List[String] = {
    val obj = getJsonFromUrl(hotelInfoUrl.format(dest.id))
    val descritpions = for {
      JObject(o) <- obj
      JField("text",JString(d)) <- o
      } yield d
      descritpions
    }
  /**
   * Retrieve JSON from given URL
   */
   private def getJsonFromUrl(url:String) : JValue = {
    parse(scala.io.Source.fromURL(url).mkString)
  }
}


