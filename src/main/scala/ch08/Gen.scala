package ch08

import ch06.*   // State

import Prop.* 

opaque type Prop = (TestCases, RNG) => Result

object Prop:
  opaque type TestCases = Int
  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase)
      def string: String = f
    def fromString(s: String): FailedCase = s

  opaque type SuccessCount = Int

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    case Proved

    def isFalsified: Boolean = this match
      case Passed => false
      case Proved => false
      case Falsified(_, _) => true

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildErrMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  import Prop.Result.{Passed, Falsified, Proved}

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    (n, rng) => randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map {
      case (a, i) => 
        try
          if f(a) then Passed else Falsified(a.toString, i)
        catch
          case e: Exception => Falsified(buildErrMsg(a, e), i)
    }.find(_.isFalsified).getOrElse(Passed)

  extension (self: Prop)
    def tag(msg: String): Prop =
      (n, rng) => self(n, rng) match
        case Falsified(e, c) => Falsified(FailedCase.fromString(s"$msg($e)"), c)
        case x => x
  
    def &&(that: Prop): Prop =
      (n, rng) => self.tag("and-left")(n, rng) match
        case Passed | Proved => that.tag("and-right")(n, rng)
        case x => x

    def ||(that: Prop): Prop =
      (n, rng) => self.tag("or-left")(n, rng) match
        case Falsified(msg, _) => that.tag("or-right")(n, rng)
        case x => x

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
