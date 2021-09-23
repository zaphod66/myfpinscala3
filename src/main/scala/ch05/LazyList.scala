package ch05

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

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val h = hd
    lazy val t = tl
    Cons(() => h, () => t)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))
