package ch08

import ch06.*   // State

import Prop.* 

opaque type Prop = (TestCases, RNG) => Result

object Prop:
  opaque type TestCases = Int
  opaque type FailedCase = String
  opaque type SuccessCount = Int

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    case Proved

    def isFalsified: Boolean = this match
      case Passed => false
      case Proved => false
      case Falsified(_, _) => true

// trait Prop:
//   def check: Either[(FailedCase, SuccessCount), SuccessCount]
//   def &&(that: Prop): Prop = ???

opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] =
      Gen.listOfN(n, self)

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)

  def unit[A](a: => A): Gen[A] = State.unit(a)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start))

  def boolean: Gen[Boolean] =
    choose(0, 2).map(i => if (i == 0) true else false)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    State.sequence(List.fill(n)(g))

  def listOfN[A](size: Gen[Int], g: Gen[A]): Gen[List[A]] =
    size.flatMap(n => listOfN(n, g))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def pair[A](g: Gen[A]): Gen[(A, A)] =
    State.map2(g)(g){ (a1, a2) => (a1, a2) }

  def stringN(len: Int): Gen[String] =
    listOfN(len, choose(0, 127).map(_.toChar)).map(_.mkString)
