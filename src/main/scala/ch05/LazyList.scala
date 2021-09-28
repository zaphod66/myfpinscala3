package ch05

import scala.{LazyList => _}

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toListSO: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toListSO

  def toList: List[A] =
    def go(ll: LazyList[A], acc: List[A]): List[A] = ll match
      case Empty      => acc.reverse
      case Cons(h, t) => go(t(), h() :: acc)

    go(this, List.empty[A])

  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, t) => Some(h())

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty

  @annotation.tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty

  def exists(p: A => Boolean): Boolean = this match
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _          => acc

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, acc) => p(a) || acc)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)

  def takeWhile2(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, acc) => if p(a) then cons(a, acc) else acc)

  def headOption2: Option[A] =
    foldRight(Option.empty[A])((a, _) => Option(a))

  def append[AA >: A](that: => LazyList[AA]): LazyList[AA] =
    foldRight(that)((a, acc) => cons(a, acc))

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, acc) => if p(a) then cons(a, acc) else acc)

  // Since intermediate lazy lists aren’t instantiated, it’s easy to
  // reuse existing combinators in novel ways without having to worry
  // that we’re doing more processing of the lazy list than necessary.
  // For example, we can reuse filter to define find, a method to return
  // just the first element that matches if it exists. Even though filter
  // transforms the whole lazy list, that transformation is done lazily,
  // so find terminates as soon as a match is found:
  def find(p: A => Boolean): Option[A] = filter(p).headOption

  final def find2(p: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) =>
      val hh = h()
      if (p(hh)) then Some(hh) else t().find2(p)

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) { s => s match
      case Empty      => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) => Some(h(), (t(), n - 1))
      case _               => None
    }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _                    => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
     unfold((this, that)) {
       case (Cons(h1, t1), Cons(h2, t2)) => Some( ((Some(h1()), Some(h2())), (t1(), t2()))  )
       case (Cons(h1, t1), Empty)        => Some( ((Some(h1()),  None)     , (t1(), Empty)) )
       case (Empty,        Cons(h2, t2)) => Some( ((None, Some(h2()))      , (Empty, t2())) )
       case (Empty,        Empty)        => None
  }

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
     unfold((this, that)) {
       case (Cons(h1, t1), Cons(h2, t2)) => Some( (f(h1(), h2()), (t1(), t2()) ))
       case _ => None
  }

  def zip[B](that: LazyList[B]): LazyList[(A, B)] =
    zipWith(that)((_, _))

  def startsWith[AA >: A](prefix: LazyList[AA]): Boolean =
    zipAll(prefix).takeWhile(_._2.isDefined).forAll( pair => pair._1 == pair._2)

  def tails: LazyList[LazyList[A]] =
    unfold(this){
      case Empty      => None
      case Cons(h, t) => Some( (cons(h(), t()), t()) )
    }.append(LazyList(empty))

  def hasSubsequence[AA >: A](pre: LazyList[AA]): Boolean =
    tails.exists(_.startsWith(pre))

  // Copy of the answer...
  // The function can't be implemented using `unfold`, since `unfold` generates elements of the `LazyList` from left to right. It can be implemented using `foldRight` though.
  // The implementation is just a `foldRight` that keeps the accumulated value and the lazy list of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(init -> LazyList(init)) { (a, b0) =>
      // b0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val b1 = b0
      val b2 = f(a, b1._1)
      (b2, cons(b2, b1._2))
    }._2

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val h = hd
    lazy val t = tl
    Cons(() => h, () => t)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    lazy val single: LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))

  def fibs: LazyList[Int] =
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))
  
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state).fold(empty)((a, s) => cons(a, unfold(s)(f)))

  def fibs2: LazyList[Int] = unfold((0, 1))((c, n) => Some(c, (n, c + n)))
  
  def from2(n: Int): LazyList[Int] = unfold(n)(s => Some(s, s + 1))
  
  def ones2: LazyList[Int] = continually2(1) // unfold(())(_ => Some(1, ()))

  def continually2[A](a: A): LazyList[A] = unfold(())(_ => Some(a, ()))

  @main
  def TestLazyList: Unit =

    val ll1 = LazyList(1, 2, 3, 4)
    val ll2 = LazyList(5, 6, 7)
    val ll3 = ll1.append(ll2)
    val ll4 = ll3.filter(_ % 2 == 0)
    val ll5 = ll3.map(_ + 1)
    val ll6 = ll3.flatMap(i => LazyList(i, i + 10))

    println(s"ll1     ${ll1.toList}")
    println(s"ll2     ${ll2.toList}")
    println(s"ll3     ${ll3.toList}")
    println(s"ll4     ${ll4.toList}")
    println(s"ll5     ${ll5.toList}")
    println(s"ll6     ${ll6.toList}")

    println(s"take    ${ones.take(5).toList}")
    println(s"ones2   ${ones2.take(5).toList}")
    println(s"cont2   ${continually2('a').take(5).toList}")
    println(s"forAll  ${ones.forAll(_ != 1)}")
    println(s"forAll- ${from(1).forAll(_ < 100)}")
    println(s"fibs    ${fibs.take(9).toList}")
    println(s"fibs2   ${fibs2.take(9).toList}")
    println(s"from2   ${from2(3).take(9).toList}")
    println(s"zip     ${ll1.zip(ll2).toList}")
    println(s"zipWith ${ll1.zipWith(ll2)(_ + _).toList}")
    println(s"zipAll  ${ll1.zipAll(ll2).toList}")
    println(s"tails   ${ll1.tails.map(_.toList).toList}")
    println(s"hasSub  ${ll1.hasSubsequence(ll1.drop(2))}")
    println(s"hasSub  ${ll1.hasSubsequence(ll2)}")
    println(s"scanR   ${ll1.scanRight(0)(_ + _).toList}")

    val ll7 = unfold(1)(s => Some(s.toString, s + 2))
    println(s"ll7     ${ll7.take(5).toList}")

    println(s"find    ${ll1.find(_ == 2)}")
    println(s"find2   ${ll1.find2(_ == 2)}")
