package ch08

import ch06.*   // State
import annotation.targetName

import Prop.* 

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x
  
  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

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

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)
  
  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildErrMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  import Prop.Result.{Passed, Falsified, Proved}

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map {
      case (a, i) => 
        try
          if f(a) then Passed else Falsified(a.toString, i)
        catch
          case e: Exception => Falsified(buildErrMsg(a, e), i)
    }.find(_.isFalsified).getOrElse(Passed)
  }

  @targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = (n.toInt - 1) / max.toInt + 1
      val props: LazyList[Prop] =
        LazyList.from(0).take((n.toInt min max.toInt) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map[Prop](p => (max, n, rng) => p(max, casesPerSize, rng)).toList.reduce(_ && _)
      prop(max, n, rng)

  extension (self: Prop)
    def tag(msg: String): Prop = 
      (max, n, rng) => self(max, n, rng) match
        case Falsified(e, c) => Falsified(FailedCase.fromString(s"$msg($e)"), c)
        case x => x
  
    def &&(that: Prop): Prop =
      (max, n, rng) => self.tag("and-left")(max, n, rng) match
        case Passed | Proved => that.tag("and-right")(max, n, rng)
        case x => x

    def ||(that: Prop): Prop =
      (max, n, rng) => self.tag("or-left")(max, n, rng) match
        case Falsified(msg, _) => that.tag("or-right")(max, n, rng)
        case x => x

    def run(maxSize: MaxSize = 100,
            testCases: TestCases = 100,
            rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      self(maxSize, testCases, rng) match
        case Falsified(msg, n) =>
          println(s"Falsified after $n passed tests: $msg.")
        case Passed =>
          println(s"Ok, passed $testCases tests.")
        case Proved =>
          println("Ok, proved")

opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] =
      Gen.listOfN(n, self)

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)

    def list: SGen[List[A]] =
      n => listOfN(n)
    
    def nonEmptyList: SGen[List[A]] =
      n => listOfN(n max 1)

    def unsized: SGen[A] = _ => self

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

// Generates samples of a given size
opaque type SGen[+A] = Int => Gen[A]

object SGen:
  def apply[A](f: Int => Gen[A]): SGen[A] = f

  def choose(start: Int, stopExclusive: Int): SGen[Int] = n => Gen.choose(start, stopExclusive)
  def boolean: SGen[Boolean] = n => Gen.boolean

object Tests:
  val smallInt = Gen.choose(-10, 10)

  val maxProp1 = Prop.forAll(smallInt.list) { ns =>
    val max = (ns).max
    !ns.exists(_ > max)
  }

  val maxProp2 = Prop.forAll(smallInt.nonEmptyList) { ns =>
    val max = (ns).max
    !ns.exists(_ > max)
  }

  val sortedProp = Prop.forAll(smallInt.list) { ns =>
    val ls = ns.sorted
    val ordered = ns.isEmpty || ls.zip(ls.tail).forall { (a, b) => a <= b }

    ordered && ns.forall(ls.contains) && ls.forall(ns.contains)
  }

  @main
  def runProps: Unit =
    println("run Props")

    maxProp1.run()  //(MaxSize.fromInt(10), TestCases.fromInt(10), RNG.Simple(42))
    println("--------------")
    maxProp2.run()
    println("--------------")
    sortedProp.run()