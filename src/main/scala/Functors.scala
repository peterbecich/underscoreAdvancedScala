import cats.Functor
import cats.instances.list._
import cats.instances.function._
import cats.syntax.functor._

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Functors extends App {

  val future1 = Future("hello world!")
  val future2 = future1.map(_.length)

  val hello1 = Await.result(future1, 1.second)
  val hello2 = Await.result(future2, 1.second)

  // functor for functions maps on output -- reader functor?
  val func1 = (x: Int) => x.toDouble
  val func2 = (y: Double) => y*2
  val func3 = func1.map(func2)

  val fourPointOh = func3(2)
  println(fourPointOh)

  val ll = (65 to 90).toList
  println(ll)
  val lc = Functor[List].map(ll)(_.toChar)
  println(lc)


}

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  implicit def treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A,B](value: Tree[A])(f: A => B): Tree[B] = value match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(left.map(f), right.map(f))
    }
  }
}

// TODO contravariant and invariant functors


