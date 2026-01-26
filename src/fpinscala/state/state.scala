package fpinscala.state

trait RNG {
    def nextInt: (Int, RNG)
}

type Rand[+A] = RNG => (A, RNG)



object RNG {
    case class SimpleRNG(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
            val nextRNG = SimpleRNG(newSeed)
            val n = (newSeed >>> 16).toInt
            (n, nextRNG)
        }
        
        val int: Rand[Int] = _.nextInt
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i, nextRNG) = rng.nextInt
        (if (i < 0) -(i + 1) else i, nextRNG)
    }

    def double(rng: RNG): (Double, RNG) = {
        val (i, r) = nonNegativeInt(rng)
        (i.toDouble / (Int.MaxValue.toDouble + 1), r)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, r1) = rng.nextInt
        val (d, r2) = double(r1)
        ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val ((i, d), r) = intDouble(rng)
        ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d1, r1) = double(rng)
        val (d2, r2) = double(r1)
        val (d3, r3) = double(r2)
        ((d1, d2, d3), r3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
        @annotation.tailrec
        def go(count: Int, r: RNG, l: List[Int]): (List[Int], RNG) =
            if (count <= 0)
                (l, r)
            else
                val (i, nextRNG) = rng.nextInt
                go(count-1, nextRNG, i :: l)
        go(count, rng, List())

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = 
        rng => (a, rng)
    
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def mapViaFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] =
        flatMap(s)(a => unit(f(a)))

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        rng => {
            val (a, r1) = ra(rng)
            val (b, r2) = rb(r1)
            (f(a, b), r2)
        }

    def map2ViaFlatmap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra)(a => map(rb)(b => f(a, b)))

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
        fs.foldRight(unit(List()): Rand[List[A]])((r, acc) => map2(r, acc)(_ :: _))

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
        rng => {
            val (a, rng2) = f(rng)
            g(a)(rng2)
        }

    def nonNegativeLessThan(n: Int): Rand[Int] = 
        flatMap(nonNegativeInt){ i => 
            val mod = i % n
            if i + (n-1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)
    }
    def nonNegativeEven: Rand[Int] =
        map(nonNegativeInt)(i => i - i % 2)

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
        map2(ra, rb)((_, _))

    def ints_1(count: Int): Rand[List[Int]] =
        sequence(List.fill(count)(int))

    val double_1: Rand[Double] =
        map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1).toDouble)

    def main(args: Array[String]): Unit = {
        val rng = SimpleRNG(55)
        println(ints_1(8)(rng))
    }
}

opaque type State[S, +A] = S => (A, S)

object State:
    extension [S, A](underlying: State[S, A])
        def run(s: S): (A, S) = underlying(s)

        def flatMap[B](f: A => State[S, B]): State[S, B] =
            s => {
                val (a, s1) = underlying(s)
                f(a)(s1)
            }

        def map[B](f: A => B): State[S, B] =
            flatMap(a => unit(f(a)))

        def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
            underlying.flatMap(a => sb.map(b => f(a, b)))

    def unit[S, A](a: A): State[S, A] =
        s => (a, s)

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
        fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_::_))
