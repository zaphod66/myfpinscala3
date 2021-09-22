package ch04

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option => _, Some => _, None => _}

enum Option[+A]:
  case Some(get: A)
  case None


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
