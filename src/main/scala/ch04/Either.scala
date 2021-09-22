package ch04

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either => _, Left => _, Right => _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)
