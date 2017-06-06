import cats.Show
import cats.Show.show // makes instances of Show typeclass

import cats.syntax.show._
import cats.instances.map._
import cats.instances.string._
import cats.instances.double._
import cats.instances.int._

import cats.Eq
import cats.syntax.eq._

import java.util.Date

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json

trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val stringJsonWriter = new JsonWriter[String] {
    def write(value: String): Json = JsString(value)
  }

  implicit val personJsonWriter = new JsonWriter[Person] {
    def write(person: Person): Json = {
      val jsName = JsString(person.name)
      val jsEmail = JsString(person.email)
      JsObject(Map(
        "name" -> jsName,
        "email" -> jsEmail))
    }
  }
}

object Json {
  import JsonWriterInstances._

  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)

  val jPat = toJson(Person("Pat", "pat@foo.com"))
  val jPeter = toJson(Person("Peter", "peter@foo.com"))

  // Implicits do *not* implicitly stack

  // implicit val showJson: Show[Json] = new Show[Json] {
  //   def show(json: Json): String = json match {
  //     case JsObject(map) => map.show
  //     case JsString(str) => str.show
  //     case JsNumber(dbl) => dbl.show
  //   }
  // }

  implicit val showJson: Show[Json] = show(
    _ match {
      case JsObject(map) => map.show
      case JsString(str) => str.show
      case JsNumber(dbl) => dbl.show
    }
  )


  val peter = jPeter.show

  val foo: String = (123.456).show
  val bar: String = Show.apply[Double].show(123.456)

  implicit val dateShow: Show[Date] =
    show(date => s"${date.getTime}ms since the epoch")

  val now = new Date()

  val nowS: String = now.show


  final case class Cat(name: String, age: Int, color: String)

  implicit val showCat: Show[Cat] = show(_ match {
    case Cat(name, age, color) => s"cat $name, $age, $color"
  })

  implicit val eqCat: Eq[Cat] = Eq.instance((_,_) match {
    case (Cat(name1, age1, color1), Cat(name2, age2, color2)) if
      name1===name2
        && age1===age2
        && color1===color2 => true
    case _ => false
  })
}
