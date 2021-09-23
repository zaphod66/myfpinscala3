package ch04

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either => _, Left => _, Right => _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(e)  => Left(e)
    case Right(a) => Right(f(a))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(e)  => Left(e)
    case Right(a) => f(a)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(_)  => b
    case Right(a) => Right(a)

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, that) match
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Right(_), Left(e))  => Left(e)
    case (Left(e), _)         => Left(e)

  def map2_2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      a <- this
      b <- that
    yield f(a, b)
  
object Either:
  def mean(xs: Seq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match
    case Nil    => Right(List.empty[B])
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)

  def traverse2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  // -------------

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if name == ""
      then Left("Name is empty.")
      else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if age < 0 || age > 120
      then Left("Age is out of range.")
      else Right(new Age(age))
  
  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  // -------------

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] = 
    (a, b) match
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(es), Right(_)) => Left(es)
      case (Right(_), Left(es)) => Left(es)
      case (Left(es1), Left(es2)) => Left(es1 ++ es2)

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = 
    as.foldRight(Right(Nil): Either[List[E], List[B]])((a, acc) => map2All(f(a), acc, _ :: _))

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] = 
    traverseAll(as, identity)

enum Validated[+E, +A]:
  case Valid(get: A)
  case Invalid(error: E)

  def toEither: Either[E, A] =
    this match
      case Valid(a) => Either.Right(a)
      case Invalid(e) => Either.Left(e)

  def map2[EE >: E, B, C](
    b: Validated[EE, B],
    f: (A, B) => C,
    combineErrors: (EE, EE) => EE
  ): Validated[EE, C] =
    (this, b) match
      case (Valid(aa), Valid(bb)) => Valid(f(aa, bb))
      case (Invalid(e), Valid(_)) => Invalid(e)
      case (Valid(_), Invalid(e)) => Invalid(e)
      case (Invalid(e1), Invalid(e2)) => Invalid(combineErrors(e1, e2))

object Validated:
  def fromEither[E, A](e: Either[E, A]): Validated[E, A] =
    e match
      case Either.Right(a) => Valid(a)
      case Either.Left(e) => Invalid(e)      

  def traverse[E, A, B](as: List[A], f: A => Validated[E, B], combineErrors: (E, E) => E): Validated[E, List[B]] =
    as.foldRight(Valid(Nil): Validated[E, List[B]])((a, acc) => f(a).map2(acc, _ :: _, combineErrors))

  def sequence[E, A](vs: List[Validated[E, A]], combineErrors: (E, E) => E): Validated[E, List[A]] =
    traverse(vs, identity, combineErrors)
