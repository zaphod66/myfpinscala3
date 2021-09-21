package ch03

enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])

object List:
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

  def dropWhile[A](as: List[A], p: A => Boolean): List[A] = as match
    case Cons(hd, tl) if p(hd) => dropWhile(tl, p)
    case _                     => as

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match
    case Nil          => a2
    case Cons(hd, tl) => Cons(hd, append(tl, a2))

  def init[A](as: List[A]): List[A] = as match
    case Nil          => sys.error("init on emtpy list")
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  
  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumWithFoldRight(ints: List[Int]): Int = foldRight(ints, 0, _ + _)
  def prodWithFoldRight(doubs: List[Double]): Double = foldRight(doubs, 1.0, _ * _)
  def lenWithFoldRight[A](as: List[A]): Int = foldRight(as, 0, (_, acc) => 1 + acc)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil         => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumWithFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0, _ + _)
  def prodWithFoldLeft(doubs: List[Double]): Double = foldLeft(doubs, 1.0, _ * _)
  def lenWithFoldLeft[A](as: List[A]): Int = foldLeft(as, 0, (acc, _) => 1 + acc)

  // reverse(Cons(1,Cons(2,Cons(3,Nil))))
  // foldLeft(Cons(1,Cons(2,Cons(3,Nil))), Nil, (acc, a) => Cons(a, acc))
  // foldLeft(Cons(2,Cons(3,Nil)), Cons(1,Nil), (acc, a) => Cons(a, acc))
  // foldLeft(Cons(3,Nil), Cons(2,Cons(1,Nil)), (acc, a) => Cons(a, acc))
  // foldLeft(Nil, Cons(3,Cons(2,Cons(1,Nil))), (acc, a) => Cons(a, acc))
  // Cons(3,Cons(2,Cons(1,Nil)))
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A], (acc, a) => Cons(a, acc))

  def foldRightWithFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def appendWithFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, Cons(_, _))

  def concat[A](ls: List[List[A]]): List[A] =
    ls match
      case Nil         => Nil
      case Cons(x, xs) => append(x, concat(xs))
  
  def concatfR[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A], append)
  
  def incrementEach(is: List[Int]): List[Int] =
    foldRight(is, Nil: List[Int], (i, acc) => Cons(i + 1, acc))

  def doubleToString(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String], (d, acc) => Cons(d.toString, acc))

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as, Nil: List[B], (a, acc) => Cons(f(a), acc))

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B], (a, acc) => append(f(a), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A], (a, acc) => if f(a) then Cons(a, acc) else acc)

  def filterFM[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match 
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))

  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = (a, b) match
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))

  def zipWithSS[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    def go(a: List[A], b: List[B], acc: List[C]): List[C] = (a, b) match 
      case (Nil, _)                     => acc
      case (_, Nil)                     => acc
      case (Cons(h1, t1), Cons(h2, t2)) => go(t1, t2, Cons(f(h1, h2), acc))

    reverse(go(a, b, Nil))

  def startsWith[A](l: List[A], pre: List[A]): Boolean = (l, pre) match
    case (_, Nil)                                 => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _                                        => false

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match
    case Nil                       => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t)                => hasSubsequence(t, sub)

  @main
  def run: Unit =
    val l1 = List(1, 2, 3)
    val l2 = List(3.1415927, 2.7182818)
    val l3 = List(List(1, 2), List(3, 4))

    println(s"sum      of $l1 = ${sum(l1)}")
    println(s"prod     of $l2 = ${product(l2)}")
    println(s"sumfR    of $l1 = ${sumWithFoldRight(l1)}")
    println(s"prodfR   of $l2 = ${prodWithFoldRight(l2)}")
    println(s"sumfL    of $l1 = ${sumWithFoldLeft(l1)}")
    println(s"prodfL   of $l2 = ${prodWithFoldLeft(l2)}")
    println(s"lenfR    of $l1 = ${lenWithFoldRight(l1)}")
    println(s"lenfR    of $l2 = ${lenWithFoldRight(l2)}")
    println(s"lenfL    of $l1 = ${lenWithFoldLeft(l1)}")
    println(s"lenfL    of $l2 = ${lenWithFoldLeft(l2)}")
    println(s"reverse  of $l1 = ${reverse(l1)}")
    println(s"concat   of $l3 = ${concat(l3)}")
    println(s"concatfR of $l3 = ${concatfR(l3)}")

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

object Tree:
  extension (t: Tree[Int]) def firstPositive: Option[Int] = t match
    case Leaf(i)      => if i > 0 then Some(i) else None
    case Branch(l, r) => l.firstPositive orElse r.firstPositive

  @main
  def runTree: Unit =
    val t1 = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
  
    println(s"t1: $t1")
    println(s"t1: ${t1.size}")
    println(s"t1: ${t1.firstPositive}")
