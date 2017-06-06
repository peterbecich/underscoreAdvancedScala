
import cats.kernel.Monoid
import cats.Eq
import cats.syntax.eq._

import cats.kernel.Semigroup

import scala.collection.Set

import cats.syntax.monoid._

import cats.instances.int._
import cats.instances.option._

object Monoids {
  def associativeLaw[A](x: A, y: A, z: A)
    (implicit m: Monoid[A], eq: Eq[A]): Boolean =
    m.combine(x, m.combine(y, z)) === m.combine(m.combine(x, y), z)

  def identityLaw[A](x: A)
    (implicit m: Monoid[A], eq: Eq[A]): Boolean =
    (m.combine(m.empty, x) === x) &&
      (m.combine(x, m.empty) === x)


}

object Semigroups {

  def associativeLaw[A](x: A, y: A, z: A)
    (implicit m: Semigroup[A], eq: Eq[A]): Boolean =
    m.combine(x, m.combine(y, z)) === m.combine(m.combine(x, y), z)

}

object Sets extends App {
  import Monoids._

  implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def combine(s1: Set[A], s2: Set[A]): Set[A] =
      s1.union(s2)
    def empty: Set[A] = Set.empty
  }


  val a = Set('a')
  val b = Set('b')
  val c = Set('c')

  val ab = Monoid[Set[Char]].combine(a, b)
  val ab2 = Monoid[Set[Char]].combine(ab, Set.empty)

  val ab3 = a |+| b

  val abc = a |+| b |+| c

  def setAdder(s: Set[Int]): Int =
    s.reduce(Monoid[Int].combine)

  def listOpAdder(loi: List[Option[Int]]): Option[Int] =
    loi.reduce(Monoid[Option[Int]].combine)

  val ll = List(Some(4), Some(5), None, Some(10))
  val ls: Option[Int] = listOpAdder(ll)
  println(ll)
  println(ls)

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def combine(o1: Order, o2: Order): Order = (o1, o2) match {
      case (Order(c1: Double, q1: Double), Order(c2: Double, q2: Double)) => Order(c1+c2, q1+q2)
    }
    def empty: Order = Order(0,0)
  }

  val o1 = Order(2, 5)
  val o2 = Order(65,9)
  val o12 = o1 |+| o2

}
