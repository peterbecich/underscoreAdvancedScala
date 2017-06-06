import cats.Monad
import cats.instances.option._
import cats.instances.list._

import cats.Eq
import cats.syntax.eq._

import cats.Eval


object OptionMonadExamples extends App {
  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if(b == 0) None else Some(a / b)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] = for {
    x <- parseInt(aStr)
    y <- parseInt(bStr)
    q <- divide(x, y)
  } yield q

  val five = stringDivideBy("10", "2")
  println(five)
}

object ListMonadExamples extends App {

  val pairs = for {
    x <- (1 to 10).toList
    y <- (30 to 35).toList
  } yield (x, y)

  println(pairs)

}

object MonadLaws {

  def associativeLaw[F[_], A, B, C](fx: F[A])(f: A => F[B], g: B => F[C])(implicit m: Monad[F], eq: Eq[F[C]]): Boolean =
    m.flatMap(m.flatMap(fx)(f))(g) ===
      m.flatMap(fx)(x => m.flatMap(f(x))(g))

  def leftIdentityLaw[F[_], A, B](x: A, f: A => F[B])
      (implicit m: Monad[F], eq: Eq[F[B]]): Boolean =
    m.flatMap(m.pure(x))(f) === f(x)

  def rightIdentityLaw[F[_], A](fx: F[A])
    (implicit m: Monad[F], eq: Eq[F[A]]): Boolean =
    m.flatMap(fx)(m.pure) === fx

}


object Evals extends App {

  val now = Eval.now {
    println("one plus two")
    1+2
  }

  println("after one plus to is eagerly evaluated")
  println(now)

  val later = Eval.later {
    println("three plus four")
    3+4
  }

  println("before three plus four is lazily evaluated")
  println(later.value)

}
