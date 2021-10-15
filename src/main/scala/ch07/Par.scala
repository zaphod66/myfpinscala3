package ch07

import java.util.concurrent.*

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  extension [A](pa: Par[A]) def map[B](f: A => B) = pa.map2(unit(()))((a, _) => f(a))

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

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def delay[A](pa: => Par[A]): Par[A] =
    es => pa(es)
  
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parSum(ints: List[Int]): Par[Int] =
    val size = ints.size
    if size <= 1000 then
      Par.unit(ints.sum)
    else
      val (l, r) = ints.splitAt(size / 2)
      val pl = Par.fork(parSum(l))
      val pr = Par.fork(parSum(r))

      pl.map2Timeout(pr)(_ + _)
    
  def parSort0(parList: Par[List[Int]]): Par[List[Int]] = parList.map2(unit(()))((a, _) => a.sorted)
  def parSort(parList: Par[List[Int]]): Par[List[Int]] = parList.map(_.sorted)

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    fork {
      val pbs: List[Par[B]] = as.map(asyncF(f))
      Par.sequence(pbs)
    }

  def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] =
    fork {
      val ppas = Par.parMap(as)( a => if p(a) then List(a) else Nil)
      ppas.map(_.flatten)
    }

  def equal[A](es: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(es).get == p2(es).get

  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if pas.isEmpty then unit(IndexedSeq.empty[A])
    else if pas.size == 1 then pas.head.map(IndexedSeq(_))
    else
      val (l, r) = pas.splitAt(pas.size / 2)
      sequenceBalanced(l).map2Timeout(sequenceBalanced(r))(_ ++ _)

  def sequence0[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((pa, acc) => pa.map2Timeout(acc)(_ :: _))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(ps.toIndexedSeq).map(_.toList)

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

    println(s"r1: $r1 ${(started2 - started1).toDouble / 1000} µs")
    println(s"r2: $r2 ${(started3 - started2).toDouble / 1000} µs")

    val i = 42
    val f: Int => String = _.toString
    val pf = Par.asyncF(f)

    val s1 = f(i)
    val s2 = pf(i).run(es).get

    println(s"s1: $s1, s2: $s2")

    val l2  = List.fill(1000)(1)
    val pl2 = Par.parMap(l2)(_.toString)
    val rl2 = pl2.run(es).get

    val l3  = List.range(1, 1000)
    val pl3 = Par.parMap(l3)(math.sqrt(_))
    val rl3 = pl3.run(es).get

    println(s"parMap: ${rl2.take(5)}")
    println(s"parMap: ${rl3.take(5)}")

    es.shutdown

    // val ftp1 = Executors.newFixedThreadPool(1)

    // val lazyVal = lazyUnit(42)

    // println(s"Deadlock?")
    // val eq = Par.equal(ftp1)(lazyVal, fork { lazyVal })
    // println(s"No! $eq")
    
    // ftp1.shutdown