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
