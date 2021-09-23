package ch04

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option => _, Some => _, None => _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(a) => Some(f(a))
    case None    => None

  def flatMap_2[B](f: A => Option[B]): Option[B] = this match
    case Some(a) => f(a)
    case None    => None

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match
    case Some(a) => a
    case None    => default

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) then Some(a) else None)

  def lift[AA >: A, B](f: AA => B): Option[AA] => Option[B] = _.map(f)

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!")
    try
      val x = 42 + 5
      x + y
    catch  case e: Exception => 43

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap( m => mean( xs.map( x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
    case Nil    => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))

  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil): Option[List[A]])((a, acc) => map2(a, acc)((h, t) => h :: t))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match
    case Nil    => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))((b, bs) => b :: bs)

  def traverse2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence3[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)

  @main
  def OptionTest: Unit =
    val l1 = List(Some(1), Some(2), Some(3))
    val l2 = List(Some(1), None, Some(3))

    println(s"$l1 : ${sequence(l1)}")
    println(s"$l1 : ${sequence2(l1)}")
    println(s"$l2 : ${sequence(l2)}")
    println(s"$l2 : ${sequence2(l2)}")

