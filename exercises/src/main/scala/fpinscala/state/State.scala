package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object Main {
  def main(args: Array[String]): Unit = {
    val rng = RNG.Simple(123)
    val (nonNeg, rng2) = RNG.nonNegativeInt(rng)
    println(nonNeg)
    val (d, rng3) = RNG.double(rng2)
    println(d)
    val ((i, d2), rng4) = RNG.intDouble(rng3)
    println(i, d2)
    val (ints, rng5) = RNG.ints(3)(rng4)
    println(ints)
    val (randomList, rng6) = RNG.sequence(List[RNG.Rand[Int]](RNG.nonNegativeInt, RNG.nonNegativeInt))(rng5)
    println(randomList)
  }

}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomNumber, nextRng) = rng.nextInt
    (scala.math.abs(randomNumber), nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (randomNumber, nextRng) = nonNegativeInt(rng)
    (randomNumber.toDouble / Int.MaxValue, nextRng)
  }

  def moreElegantDouble = {
    map(nonNegativeInt)(_.toDouble)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intVal, newRng) = nonNegativeInt(rng)
    val (doubleVal, neewRng) = double(newRng)
    ((intVal, doubleVal), neewRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (doubleVal, newRng) = double(rng)
    val (intVal, neewRng) = nonNegativeInt(newRng)
    ((doubleVal, intVal), neewRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doubleVal, newRng) = double(rng)
    val (dooubleVal, neewRng) = double(newRng)
    val (doooubleVal, neeewRng) = double(neewRng)
    ((doubleVal, dooubleVal, doooubleVal), neeewRng)
  }

  private def helpMeGodWithListState(
      count: Int,
      in: List[Int],
      rng: RNG
  ): (List[Int], RNG) = {
    if (count == 0) (in, rng)
    else {
      val (newInt, newRNG) = nonNegativeInt(rng)
      helpMeGodWithListState(count - 1, newInt :: in, newRNG)
    }
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    helpMeGodWithListState(count, List.empty, rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (a, rngA) = ra(rng) //not an object, but actually a function!
        val (b, rngB) = rb(rngA)
        (f(a, b), rngB) //Rand[C]
      }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def sequenceStep(
        fs: List[Rand[A]],
        in: List[A],
        rng: RNG
    ): (List[A], RNG) = {
      fs match {
        case Nil => (in, rng)
        case randA :: tail =>
          val (a, newRNG) = randA(rng)
          sequenceStep(tail, a :: in, newRNG)
      }
    }
    //i need to return a rand
    rng => sequenceStep(fs, List.empty, rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
