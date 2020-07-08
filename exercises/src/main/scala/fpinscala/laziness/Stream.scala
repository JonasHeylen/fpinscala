package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    //foldRight(List.empty[A])((elem, acc) => acc :+ elem)
    this match {
      case Cons(h, t) =>  h() +: t().toList 
      case Empty => List.empty
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
      case _ => Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n == 0 => Cons(h,t)
      // case Cons(h,t) if n == 1 => t()\||||\\\\\\\\\\[[[[[[[]][]]]]]]
      case Cons(h,t) => t().drop(n-1)
      case Empty => Empty
    } 
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    //   this match {
    //   case Cons(h, t)  if p(h()) => Cons(h, () => t().takeWhile(p))
    //   case _ => Empty
    // }
    this.foldRight(Stream.empty[A]){(elem,acc) => 
      if(p((elem))) Cons(() => elem, () => acc)
      else acc
    }
  }////''''''/''/                                                                                                 

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) if p(h()) => t().forAll(p)
      case Empty => true //old phylosophical connundrum
      case _ => false
    }
  }

  def headOption: Option[A] = {
    this match {
      case Cons(h, t) => Some(h())
      case Empty => None
    }
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = {
    this.foldRight(Stream.empty[B]){
      (elem, acc) => Cons(() => f(elem),() => acc)
    }
  }

  def filter(f: A => Boolean): Stream[A] = {
    this.foldRight(Stream.empty[A]){
      (elem,acc) => if(f(elem)) Cons(() => elem, () => acc)
      else acc
    }
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    this.foldRight(s){
      (elem,acc) => Cons(() => elem, () => acc)
    }
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    this.foldRight(Stream.empty[B]){
      (elem,acc) => f(elem).append(acc)
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a:A):Stream[A] = Stream.cons(a,constant(a)) //second parameter of cons is lazy -> it will only compute next value if needed
    
  def from(n: Int): Stream[Int] = cons(n,from(n+1))  //corecursive!!!!

  def fibs(n:Int,z:Int): Stream[Int] = cons(n+z,fibs(z,n+z))
  def fibs: Stream[Int] = cons(0, cons(1, fibs(0,1)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((nextValue, nextState)) => cons(nextValue,unfold(nextState)(f)) //you need to create a stream, so that's why you call cons
    case None => Empty
  }

  def from2(n:Int):Stream[Int] = unfold(n)(z => Some(z,z+1))
  val fibs2:Stream[Int] = unfold((0,1)){ case (z,y) => Some((z+y,(y, z+y)))}
  def constant2[A](a:A):Stream[A] = unfold(())(z => Some(a,()))
  val ones2: Stream[Int] = unfold(())(_ => Some(1,()))
}

object StreamMain {
  def main(args: Array[String]): Unit = {
    // println(Stream(1, 2, 3).take(2).toList)
    // println(Stream(1, 2, 3).drop(2).toList)
    // println(Stream(1, 2, 3).takeWhile(_ < 3).toList)
    // println(Stream(1, 2, 3).forAll(_ < 3))
    // println(Stream(1, 2, 3).forAll(_ < 5))
    // println(Stream(1, 2, 3).headOption)
    // println(Stream(1, 2, 3).map(_ +2).toList)
    // println(Stream(1, 2, 3).filter(_ < 5).toList)
    // println(Stream(1, 2, 3).append(Stream(4,5,6)).toList)
    // println(Stream(1, 2, 3).flatMap(x => Stream(x, x)).toList)

    //println(Stream.constant("a").take(3).toList)
    // println(Stream.from(1).take(4).toList)
    println(Stream.fibs2.take(10).toList)
    println(Stream.constant2(2).take(10).toList)
    println(Stream.ones2.take(3).toList)
    println(Stream.unfold(1)(z => Some(z,z+1)).take(10).toList)
  }
}