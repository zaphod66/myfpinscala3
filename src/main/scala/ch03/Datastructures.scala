// package ch03

enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])

object List {
  def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(ints: List[Double]): Double = ints match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def tail[A](as: List[A]): List[A] = as match
    case Cons(_, t) => t
    case Nil        => sys.error("tail on empty list")

  def setHead[A](as: List[A], hd: A): List[A] = as match
    case Cons(_, tl) => Cons(hd, tl)
    case Nil         => sys.error("setHead on empty list")

  def drop[A](as: List[A], n: Int): List[A] =
    if n <= 0 then as
    else as match
      case Cons(_, tl) => drop(tl, n - 1)
      case Nil         => Nil


  @main
  def run: Unit =
    val l1 = List(1, 2, 3)
    val l2 = List(3.1415927, 2.7182818)

    val s1 = sum(l1)
    val p1 = product(l2)

    println(s"sum  of $l1 = $s1")
    println(s"prod of $l2 = $p1")
}
