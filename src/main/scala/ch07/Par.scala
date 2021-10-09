package ch07

import java.util.concurrent.*

opaque type Par[A] = ExecutorService => Future[A]

