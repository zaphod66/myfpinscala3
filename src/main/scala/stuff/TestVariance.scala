package stuff

class Stack[+A]:
    def top: A = throw new RuntimeException("no element on stack")
    def pop: Stack[A] = throw new RuntimeException("no element on stack")
    def push[B >: A](b: B): Stack[B] = new Stack[B]:
        override def top: B = b
        override def pop: Stack[B] = Stack.this
        override def toString: String = s"${b.toString} -> ${Stack.this.toString}"

    override def toString: String = "<>"

object TestVariance:
    @main
    def variance: Unit =
        val s0: Stack[Any] = new Stack
        val s1 = s0.push("Hello")
        val s2 = s1.push(new Object())
        val s3 = s2.push(42)

        println(s"s0 = $s0")
        println(s"s1 = $s1")
        println(s"s2 = $s2")
        println(s"s3 = $s3")
