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

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val h = hd
    lazy val t = tl
    Cons(() => h, () => t)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  @main
  def TestLazyList: Unit =
    def printLL[A](str: String, ll: LazyList[A]): Unit =
      println(s"$str - ${ll.toList}")

    val ll1 = LazyList(1, 2, 3, 4)
    val ll2 = LazyList(5, 6, 7, 8)
    val ll3 = ll1.append(ll2)
    val ll4 = ll3.filter(_ % 2 == 0)
    val ll5 = ll3.map(_ + 1)
    val ll6 = ll3.flatMap(i => LazyList(i, i + 10))

    printLL("ll1", ll1)
    printLL("ll2", ll2)
    printLL("ll3", ll3)
    printLL("ll4", ll4)
    printLL("ll5", ll5)
    printLL("ll6", ll6)

    println(s"take   ${ones.take(5).toList}")
    println(s"forAll ${ones.forAll(_ != 1)}")
