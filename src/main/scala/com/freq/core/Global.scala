import scala.io
import org.json4s._
import org.json4s.native.JsonMethods._
import java.io.FileNotFoundException

case class Destination(name:String,id:Int)

object Global extends App{

  val dest = args.toList
  println(s"Started with args $dest" )
  for(d <- dest){
    try {
      println(DestinationTools.getInfo(d))
      } catch {
        case e : FileNotFoundException => println(s"$d id not found!")
        case e : Exception => println("Error")
      }
  }
}

object DestinationTools{
  //URL to get information of destionation
  val destinationUrl = "http://concierge.top10.com/v1/destinations/%s"

  def getInfo(destId:String) : Destination = {
    val obj = parse(scala.io.Source.fromURL(destinationUrl.format(destId)).mkString)

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


