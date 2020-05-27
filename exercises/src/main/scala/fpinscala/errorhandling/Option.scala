package fpinscala.errorhandling

import scala.{
  Option => _,
  Some => _,
  Either => _,
  _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None      => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None      => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(smth => f(smth)).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(v => Some(v)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => Some(get)
    case _                   => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    // mean(xs).flatMap { m =>
    //   mean(xs.map(x => math.pow(x - m, 2)))
    // }
    for {
      m <- mean(xs)
      variance <- mean(xs.map(x => math.pow(x - m, 2)))
    } yield variance
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    // a.flatMap(wht => b.map(smth => f(wht,smth)))
    for {
      wht <- a
      smth <- b
    } yield {
      f(wht, smth)
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    // a.foldLeft[Option[List[A]]](Some(List.empty[A])) {
    //   case (acc, element) =>
    //     map2(acc, element) { case (acc, element) => 
    //       acc :+ element }
    // }
    traverse(a)(opt => opt)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    //sequence(a.map(elem =>f(elem)))
      a.foldLeft[Option[List[B]]](Some(List.empty[B])) {
      case (acc, element) =>
        map2(acc, f(element)) { case (acc, element) => 
          acc :+ element }
    }
  }
  

}

object OptionMain {
  def main(args: Array[String]): Unit = {
    println(Some(1).map(_ + 1))
    println(Some(1).flatMap(x => Some(x + 1)))
    println(Some(1).orElse(Some(2)))
    println(None.orElse(Some(2)))

    println(Some(3).filter(_ > 1))
    println(Some(3).filter(_ > 10))
  }
}
