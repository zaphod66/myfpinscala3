package ch06

trait RNG:
  def nextInt: (Int, RNG)

object RNG:
  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n       = (newSeed >>> 16).toInt

      (n, nextRNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)

  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)

  def intDouble(r0: RNG): ((Int, Double), RNG) =
    val (i, r1) = r0.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)

  def double3(r0: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = double(r0)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def intsSO(count: Int)(rng: RNG): (List[Int], RNG) =
    if count == 0 then
      (List.empty[Int], rng)
    else
      val (i, r1) = rng.nextInt
      val (is, r2) = ints(count - 1)(r1)
      (i :: is, r2)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    def go(c: Int, r0: RNG, acc: List[Int]): (List[Int], RNG) =
      if c <= 0 then
        (acc, r0)
      else
        val (i, r1) = r0.nextInt
        go(c - 1, r1, i :: acc)

    go(count, rng, List.empty[Int])

  ////////////////////////////////

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng =>
    val (a, r) = s(rng)
    (f(a), r)

  def nonNegativeEvenInt: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng0 =>
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double2)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double2, int)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    def go(xs: List[Rand[A]], acc: Rand[List[A]]): Rand[List[A]] = xs match
      case Nil       => acc
      case ra :: ras =>
        val acc2 = map2(ra, acc)(_ :: _)
        go(ras, acc2)

    go(rs, unit(List.empty[A]))

  def sequenceBook[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A]))((r,acc) => map2(r, acc)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] = rng =>
    val (a, rng2) = ra(rng)
    f(a)(rng2)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)
    }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb) (b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  ////////////////////////////////

  // case class State[S, +A](run: S => (A, S))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

    def _map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = s0 =>
      val (a, s1) = underlying(s0)
      val (b, s2) = sb(s1)
      (f(a, b), s2)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] = s0 =>
      val (a, s1) = underlying(s0)
      f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight(unit(List.empty[A]))((s, acc) => s.map2(acc)(_ :: _))

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit(List.empty[B]))((a, acc) => f(a)._map2(acc)(_ :: _))
  
  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)
  
  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()
  
  ////////////////////////////////

  enum Input:
    case Coin, Turn

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def update(i: Input)(s: Machine): Machine = (i, s) match
    case (_, Machine(_, 0, _))                        => s
    case (Input.Coin, Machine(false, _, _))           => s
    case (Input.Turn, Machine(true, _, _))            => s
    case (Input.Coin, Machine(true, candies, coins))  => Machine(false, candies, coins + 1)
    case (Input.Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
  //   case _                                            => s

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.traverse(inputs)(i => State.modify(update(i)))
      s <- State.get
    yield (s.coins, s.candies)

  ////////////////////////////////
  
  @main
  def run: Unit =
    import RNG.*
    
    type Rnd[A] = State[RNG, A]

    val ns: Rnd[List[Int]] =
      for
        x <- nonNegativeLessThan(20)
        y <- nonNegativeLessThan(1000)
        xs <- ints(x).map{ xs => xs.map( Math.abs ) }
      yield xs.map(_ % y)
  
    val simple = Simple(15)
    val ls = ns(simple)._1

    println(s"ls: $ls")

    val inputs = ints(20).map(l => l.map(_ % 2).map(i => if (i % 2 == 0) Input.Coin else Input.Turn))(simple)._1

    val (coins, candies) = simulateMachine(inputs).run(Machine(true, 10, 0))._1

    println(s"inputs: $inputs")
    println(s"candies: $candies, coins: $coins")
