package ch07

import java.util.concurrent.*

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  extension [A](pa: Par[A]) def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get))

  extension [A](pa: Par[A]) def map2Timeout[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C]:
      private val af = pa(es)
      private val bf = pb(es)

      @volatile
      private var cache: Option[C] = None

      def isDone = cache.isDefined
      def get() = get(Long.MaxValue, TimeUnit.NANOSECONDS)
      def get(timeout: Long, units: TimeUnit) =
        val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
        val started      = System.nanoTime

        val a = af.get(timeoutNanos, TimeUnit.NANOSECONDS)
        val elapsed = System.nanoTime - started
        val b = bf.get(timeoutNanos - elapsed, TimeUnit.NANOSECONDS)

        val c = f(a, b)
        cache = Some(c)

        c
      
      def isCancelled = af.isCancelled || bf.isCancelled

      def cancel(evenIfRunning: Boolean): Boolean = 
        af.cancel(evenIfRunning) || bf.cancel(evenIfRunning)

  def fork[A](pa: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] { def call = pa(es).get })

  def parSum(ints: List[Int]): Par[Int] =
    val size = ints.size
    if size <= 1000 then
      Par.unit(ints.sum)
    else
      val (l, r) = ints.splitAt(size / 2)
      val pl = Par.fork(parSum(l))
      val pr = Par.fork(parSum(r))

      pl.map2Timeout(pr)(_ + _)
    
  @main
  def run: Unit =
    println(s"runPar")
    // val es = Executors.newCachedThreadPool
    val es = Executors.newWorkStealingPool

    val l1 = List.fill(10000000)(1)

    val started1 = System.nanoTime
    val r1 = l1.sum
    val started2 = System.nanoTime
    val r2 = parSum(l1).run(es).get
    val started3 = System.nanoTime

    println(s"r1: $r1 ${(started2 - started1).toDouble / 1000} ms")
    println(s"r2: $r2 ${(started3 - started2).toDouble / 1000} ms")

    es.shutdown
