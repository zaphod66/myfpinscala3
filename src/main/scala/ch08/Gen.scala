package ch08

import ch06.*   // State
import ch07.Par // Parallelism

import annotation.targetName

import Prop.* 
import Gen.*

import java.util.concurrent.{ExecutorService, Executors}

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

  def check(p: => Boolean): Prop =
    (_, _, _) => if p then Proved else Falsified("check", 0)

  val executors: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  // def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
  //   forAll(executors ** g)((e, a) => f(a).run(e).get)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(executors ** g) { case e ** a => f(a).run(e).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)
  
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

    def map[B](f: A => B): Gen[B] =
      State.map(self)(f)
    
    def map2[B, C](that: Gen[B])(f: (A, B) => C): Gen[C] =
      State.map2(self)(that)(f)

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)

    def list: SGen[List[A]] =
      n => listOfN(n)
    
    def nonEmptyList: SGen[List[A]] =
      n => listOfN(n max 1)

    def unsized: SGen[A] = _ => self

    @targetName("product")
    def **[B](gb: Gen[B]): Gen[(A, B)] = map2(gb)((_, _))

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

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(d => if d < g1Threshold then g1._1 else g2._1)

  def pair[A](g: Gen[A]): Gen[(A, A)] =
    State.map2(g)(g){ (a1, a2) => (a1, a2) }

  def stringN(len: Int): Gen[String] =
    listOfN(len, choose(0, 127).map(_.toChar)).map(_.mkString)

  object ** :
    def unapply[A, B](p: (A, B)) = Some(p)
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

  val executor: ExecutorService = Executors.newCachedThreadPool
  
  val parProp1 = check {
    val p1 = Par.unit(1).map(_ + 1)
    val p2 = Par.unit(2)
    p1.run(executor).get == p2.run(executor).get
  }

  def eq[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
    p1.map2(p2)(_ == _)
  
  val parProp2 = check {
    eq(
      p1 = Par.unit(1).map(_ + 1),
      p2 = Par.unit(2)
    ).run(executor).get
  }

  val parProp3 = checkPar {
    eq (
      Par.unit(1).map(_ + 1),
      Par.unit(2)
    )
  }

  @main
  def runProps: Unit =
    println("run Props")

    maxProp1.run()  //(MaxSize.fromInt(10), TestCases.fromInt(10), RNG.Simple(42))
    println("--------------")
    maxProp2.run()
    println("--------------")
    sortedProp.run()
    println("--------------")
    parProp1.run()
    println("--------------")
    parProp2.run()
    println("--------------")
    parProp3.run()
