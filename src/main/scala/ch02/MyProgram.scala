package ch02

object MyProgram:
    def abs(n: Int): Int =
        if n < 0 then -n
        else n

    def factorial(n: Int): Int =
        @annotation.tailrec
        def go(n: Int, acc: Int): Int =
            if n <= 0 then acc
            else go(n - 1, n * acc)
        
        go(n, 1)
    
    def fib(n: Int): Int =
        @annotation.tailrec
        def go(n: Int, curr: Int, next: Int): Int =
            if n <= 0 then curr
            else go(n - 1, next, curr + next)
        
        go(n, 0, 1)

    private def formatResult(name: String, n: Int, f: Int => Int) =
        val msg = "The %s of %d is %d."
        msg.format(name, n, f(n))

    @main
    def printAbsFac: Unit =
        println(formatResult("absolute value", -42, abs))
        println(formatResult("factorial", 5, factorial))
        println(formatResult("fibonacci", 15, fib))

object PolymorphicFunctions:
    def findFirst[A](as: Array[A], p: A => Boolean): Option[Int] =
        @annotation.tailrec
        def go(n: Int): Option[Int] =
            if n >= as.length then None
            else if p(as(n)) then Some(n)
            else go(n + 1)
        
        go(0)

    def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
        @annotation.tailrec
        def go(n: Int): Boolean =
            if n + 1 >= as.length then true
            else if gt(as(n), as(n + 1)) then false
            else go(n + 1)
        
        go(0)

    def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
    def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

    @main
    def call: Unit =
        println(findFirst(Array(2, 3, 42, 3), _ == 3))
        println(isSorted(Array(2, 3, 42, 3), _ > _))
        println(isSorted(Array(2, 3, 42, 43), _ > _))

        // val cadd = curry(add)
        val cadd = curry((a: Int, b: Int) => a + b)
        val cada = cadd(1)
        val uadd = uncurry(cadd)

        println(s"curried:   ${cada(2)}")
        println(s"uncurried: ${uadd(1, 2)}")
    