package stuff


import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

object TestContext:
  object FutureCF:
    type Executable[T] = ExecutionContext ?=> T                        // <1>
  
    def apply[T](body: => T): Executable[Future[T]] = Future(body)     // <2>
  
  def sleepN(dur: Duration): Duration =                                // <3>
    val start = System.currentTimeMillis()
    Thread.sleep(dur.toMillis)
    Duration(System.currentTimeMillis - start, MILLISECONDS)
  
  val future1 = FutureCF(sleepN(1.second))                             // <4>
  val future2 = FutureCF(sleepN(1.second))(using global)
  val duration1 = Await.result(future1, 2.seconds)                     // <5>
  val duration2 = Await.result(future2, 2.seconds)

  def name[A](implicit m: scala.reflect.Manifest[A]) = m.toString
  def etype[A](implicit m: scala.reflect.Manifest[A]) = m.runtimeClass

  @main
  def runContext: Unit =
    println(s"future1: $future1")
    println(s"future2: $future2")
    println(s"duration1: $duration1")
    println(s"duration2: $duration2")

    // println(s"name[Int => Double] -> ${name[Int => Int]}")
    // println(s"etype[Int => Double] -> ${etype[Int => Double]}")
