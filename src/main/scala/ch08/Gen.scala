package ch08

import ch06.*   // State

opaque type Gen[+A] = State[RNG, A]

object Gen:
  def unit[A](a: => A): Gen[A] = State.unit(a)
