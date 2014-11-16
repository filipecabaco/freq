import scala.io
import scala.concurrent._
import scala.util.{Success, Failure}

import org.json4s._
import org.json4s.native.JsonMethods._

import ExecutionContext.Implicits.global
import java.io.FileNotFoundException


case class Destination(name:String,id:Int)

object Global extends App{

  val dest = args.toList
  println(s"Started with args $dest" )

  val futures = dest.map{d=>Future(Tools.getDestInfo(d))}
  futures.map( _ onComplete{
    case Success(value) => println(value)
    case Failure (t) => println("Error thrown - %s".format(t.getClass().getSimpleName()))
    })
}

object Tools{
  //URL to get information of destionation
  val destinationInfoUrl = "http://concierge.top10.com/v1/destinations/%s"

  def getDestInfo(destId:String) : Destination = {
    val obj = parse(scala.io.Source.fromURL(destinationInfoUrl.format(destId)).mkString)

    //Extract name from JSON
    val name = for {
      JObject(elem) <- obj
      JField("id",JString(id)) <- elem
      if id == destId
      JField("name",JString(n)) <- elem
    } yield n

    new Destination(name.mkString,destId.toInt)
  }
}


