package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = {
   this match {
     case Left(get) => Left(get)
     case Right(get) => Right(f(get))
   }
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
   this match {
     case Left(get) => Left(get)
     case Right(get) => f(get)
   }
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
   this match {
     case Left(get) => b
     case Right(get) => Right(get)
   }
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
  //  this match {
  //    case Left(get) => Left(get)
  //    case Right(get) => b map(smth => f(get,smth))
  //  }
  this flatMap(smth => b map(whtv => f(smth,whtv)))
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    //es.foldLeft[Either[E, List[B]]](Right(List.empty[B]))((acc,elem) => acc.map2(f(elem))((e,b) => e :+ b))
    es.foldLeft[Either[E, List[B]]](Right(List.empty[B]))((acc,elem) => acc.flatMap(smth => f(elem) map (whtv => smth :+ whtv)))
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

// https://typelevel.org/cats/datatypes/validated.html

object EitherMain {
  def main(args: Array[String]): Unit = {
    val left: Either[String, Int] = Left[String]("error")
    val right = Right(10)
    println(right.map(_ + 10))
    println(left.map(_ + 10))

    println(right.flatMap(x => Right(x * 2)))
    println(right.flatMap(x => Left("no")))

    println(right.orElse({println("hello1"); Right(1)}))
    println(left.orElse({println("hello2"); Right(1)}))

    println(right.map2(Right(2))((a, b) => a + b))

    val result = Either.traverse(List(1,2,3))(x => Right(x * 2))
    val resultFail = Either.traverse(List(1,2,3))(x => if (x < 3) Right(x * 2) else Left("no"))
    println(result)
    println(resultFail)
  }
}