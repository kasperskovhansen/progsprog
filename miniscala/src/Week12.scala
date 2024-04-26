object Week12 {

  def main(args: Array[String]): Unit = {

    /**
     * Lists.
     */
    sealed abstract class List[+T] {

      def filter(p: T => Boolean): List[T] = this match {
        case Nil => Nil
        case Cons(y, ys) =>
          val r = ys.filter(p)
          if (p(y)) Cons(y, r) else r
      }

      def map[U](f: T => U): List[U] = this match {
        case Nil => Nil
        case Cons(y, ys) => Cons(f(y), ys.map(f))
      }

      def foldRight[B](z: B, f: (T, B) => B): B = this match {
        case Nil => z
        case Cons(h, t) => f(h, t.foldRight(z, f))
      }
    }

    case object Nil extends List[Nothing]

    case class Cons[T](x: T, xs: List[T]) extends List[T]


    /**
     * Streams.
     */
    sealed abstract class Stream[+T] {

      def head(): T = this match {
        case SNil => throw new RuntimeException("stream is empty")
        case SCons(x, _) => x()
      }

      def tail(): Stream[T] = this match {
        case SNil => throw new RuntimeException("stream is empty")
        case SCons(_, xs) => xs()
      }

      def map[U](f: T => U): Stream[U] = this match {
        case SNil => SNil
        case SCons(x, xs) => SCons(() => f(x()), () => xs().map(f))
      }

      def foreach(f: T => Unit): Unit = this match {
        case SNil =>
        case SCons(x, xs) =>
          f(x())
          xs().foreach(f)
      }

      def filter(p: T => Boolean): Stream[T] = ???

      def zip[U](ys: Stream[U]): Stream[(T, U)] = ???

      def take(n: Int): Stream[T] =
        if (n == 0) SNil else SCons(() => head(), () => tail().take(n - 1))

      def foldRight[B](z: () => B, f: (T, () => B) => B): B = this match {
        case SNil => z()
        case SCons(h, t) => f(h(), () => t().foldRight(z, f))
      }

      def toList(): List[T] = this match {
        case SNil => Nil
        case SCons(x, xs) => Cons(x(), xs().toList())
      }

    }

    case object SNil extends Stream[Nothing]

    case class SCons[T](x: () => T, xs: () => Stream[T]) extends Stream[T]


    val s1: Stream[Int] =
      SCons(() => 1, () =>
        SCons(() => 2, () =>
          SCons(() => 3, () =>
            SNil)))

    println(s"first element of s1: ${s1.head()}")
    println(s"second element of s1: ${s1.tail().head()}")

    def listToStream[T](xs: List[T]): Stream[T] = xs match {
      case Nil => SNil
      case Cons(y, ys) => SCons(() => y, () => listToStream(ys))
    }

    val ones: Stream[Int] = {
      def gen(): Stream[Int] = SCons(() => 1, () => gen())

      gen()
    }

    val nats: Stream[Int] = {
      def gen(n: Int): Stream[Int] = SCons(() => n, () => gen(n + 1))

      gen(0)
    }

    val fibs: Stream[Int] = {
      def gen(a: Int, b: Int): Stream[Int] = SCons(() => a, () => gen(a + b, a))

      gen(0, 1)
    }

    def zip[T, U](xs: List[T], ys: List[U]): List[(T, U)] = (xs, ys) match
      case (Cons(a, as), Cons(b, bs)) => Cons((a, b), zip(as, bs))
      case _ => Nil

    def unzip[T, U](zs: List[(T, U)]): (List[T], List[U]) = zs match
      case Nil => (Nil, Nil)
      case Cons(x, xs) =>
        val next = unzip(xs)
        (Cons(x._1, next._1), Cons(x._2, next._2))

    val listA: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val listB: List[Double] = Cons(1.5, Cons(2.5, Cons(3.5, Nil)))

    println("Zip list:")
    val zipped = zip(listA, listB)
    println(s"Zipped: $zipped")
    val unzipped = unzip(zipped)
    println(s"Unzipped: $unzipped")

    val cyclic: Stream[Int] = {
      def gen(a: Int): Stream[Int] = SCons(() => a, () => gen((a-1).abs))
      gen(0)
    }

    println("Cyclic:")
    cyclic.take(10).foreach(println)

    println("Fibonacci:")
    fibs.take(25).foreach(println)

    //    println("Nats:")
    //    nats.filter(x => x == 3).take(1).foreach(println)

    def sieve(xs: Stream[Int]): Stream[Int] =
      SCons(() => xs.head(), () => sieve(xs.tail().filter(x => x % xs.head() != 0)))

    val primes = sieve(nats.tail().tail())

    println("Primes:")
    primes.take(100).foreach(println)

    def fibs2(): Stream[Int] =
      SCons(() => 0, () =>
        SCons(() => 1, () =>
          fibs2().zip(fibs2().tail()).map(n => n._1 + n._2)))

    println("Fibonacci again:")
    fibs2().take(25).foreach(println)

    println(s"Fibonacci as list: ${fibs2().take(25).toList()}")

    class Sighting(
                    val animal: String, // Which animal
                    val spotter: Int, // Who saw it
                    val count: Int, // How many
                    val area: Int, // Where
                    val period: Int // When
                  )

    val sightings: List[Sighting] =
      Cons(new Sighting("Elephant", 17, 5, 3, 1),
        Cons(new Sighting("Lion", 2, 5, 3, 2),
          Cons(new Sighting("Elephant", 2, 3, 3, 2),
            Nil)))

    val elephants1: Int =
      sightings.filter(s => s.animal == "Elephant")
        .map(s => s.count)
        .foldRight(0, (x: Int, res: Int) => x + res)
    println(s"Elephant sightings: $elephants1")

    val elephants2: Int =
      listToStream(sightings).filter(s => s.animal == "Elephant")
        .map(s => s.count)
        .foldRight(() => 0, (x: Int, res: () => Int) => x + res())
    println(s"Elephant sightings: $elephants2")

  }

}


