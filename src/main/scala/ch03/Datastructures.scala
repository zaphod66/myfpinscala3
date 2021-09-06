package ch03

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

  @main
  def run: Unit =
    val l1 = List(1, 2, 3)
    val l2 = List(3.1415927, 2.7182818)

    val s1 = sum(l1)
    val p1 = product(l2)

    println(s"sum  of $l1 = $s1")
    println(s"prod of $l2 = $p1")
}
