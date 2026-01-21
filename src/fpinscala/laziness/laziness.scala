package fpinscala.laziness

sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
    }

    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if (n > 1) => Stream.cons(h(), t().take(n - 1))
        case Cons(h, t) if (n == 1) => Stream.cons(h(), Stream.empty)
        case _ => Stream.empty
    }

    def take_1(n: Int): Stream[A] = 
        Stream.unfold((this, n)){
            case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
            case _ => None
        }

    // take_1跟take_2最大的差別在語意而不是效能
    // 我們使用functional programming的概念的時候，要寫的declarative
    // 重點在於要表達termination的state，讓閱讀者不用自行模擬運行時的樣貌
    def take_2(n: Int): Stream[A] = 
        Stream.unfold((this, n)){
            case (Cons(h, t), n) if n == 1 => Some((h(), (Stream.empty, 0)))
            case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
            case _ => None
        }

    def drop(n: Int): Stream[A] = this match {
        case Cons(_, t) if (n > 0) => t().drop(n-1)
        case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
        case _ => Stream.empty
    }

    def takeWhile_1(p: A => Boolean): Stream[A] = 
        foldRight(Stream.empty: Stream[A])((a, b) => if (p(a)) then Stream.cons(a, b.takeWhile(p)) else Stream.empty)

    def takeWhile_2(p: A => Boolean): Stream[A] = 
        Stream.unfold(this){
            case Cons(h, t) if p(h()) => Some((h(), t()))
            case _ => None
        }
    
    def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
        Stream.unfold((this, s2)){
            case (Cons(h1, t1), Cons(h2, t2)) => (Some((f(h1(), h2()), (t1(), t2()))))
            case _ => None
        }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
        Stream.unfold((this, s2)){
            case (Cons(h1, t1), Cons(h2, t2)) => (Some((Some(h1()), Some(h2())), (t1(), t2()) ))
            case (Cons(h1, t1), Empty) => (Some((Some(h1()), None), (t1(), Empty)))
            case (Empty, Cons(h2, t2)) => (Some((None, Some(h2())), (Empty, t2())))
            case _ => None
        }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = 
        this match {
            case Cons(h, t) => f(h(), t().foldRight(z)(f))
            case _ => z
        }

    def map[B](f: A => B): Stream[B] =
        foldRight(Stream.empty: Stream[B])((a, acc) => Stream.cons(f(a), acc))
    
    def map_1[B](f: A => B): Stream[B] ={
        Stream.unfold(this)(a => a match 
            case Empty => None
            case Cons(h, t) => Some(f(h()), t())
        )
    }
    
    def filter(f: A => Boolean): Stream[A] = 
        foldRight(Stream.empty: Stream[A])((a, acc) => if f(a) then Stream.cons(a, acc) else acc)

    def append[A2 >: A](that: => Stream[A2]): Stream[A2] =
        foldRight(that)((a, acc) => Stream.cons(a, acc))

    def flatMap[B](f: A => Stream[B]): Stream[B] = 
        foldRight(Stream.empty)((a, acc) => f(a).append(acc))

    def exists(p: A => Boolean): Boolean = 
        foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
        foldRight(true)((a, b) => p(a) && b)

    def forAll_1(p: A => Boolean): Boolean = this match {
        case Cons(h, t) => p(h()) && t().forAll_1(p)
        case _ => true
    }

    def headOption_1: Option[A] = 
        foldRight(None: Option[A])((h, t) => Some(h))

    def startsWith[A](s: Stream[A]): Boolean =
        zipAll(s).takeWhile_2(xs => !xs._2.isEmpty).forAll((a, b)=> a == b)
    
    def tails: Stream[Stream[A]] =
        Stream.unfold(this)( s => s match {
                case Cons(h, t) => Some((Cons(h, t), t()))
                case _ => None
            }
        ).append(Stream(Stream.empty))

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
        foldRight((z, Stream(z)))((a, p0) => {
            lazy val p1 = p0
            val b2 = f(a, p1._1)
            (b2, Stream.cons(b2, p1._2))
        })._2

    def hasSubsequence[A](s: Stream[A]): Boolean =
        tails.exists( x => x.startsWith(s))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }
    
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail*))

    def constant[A](a: A): Stream[A] = 
        Stream.cons(a, Stream.constant(a))

    // This is more efficient than `cons(a, constant(a))` since it's just
    // one object referencing itself.
    def constant_1[A](a: A): Stream[A] = 
        lazy val tail: Stream[A] = Cons(() => a, () => tail)
        tail

    def constant_2[A](a: A): Stream[A] = 
        unfold(a)(_ => Some((a, a)))

    def ones: Stream[Int] =
        unfold(1)(_ => Some(1, 1))

    def from(n: Int): Stream[Int] =
        Stream.cons(n, from(n + 1))
    
    def from_1(n: Int): Stream[Int] = 
        unfold(n){h => Some((h, h+1))}

    def fibs: Stream[Int] = {
        def go(current: Int, next:Int): Stream[Int] = 
            Stream.cons(current, go(next, current+next))
        go(0, 1)
    }

    def fibs_1: Stream[Int] = {
        unfold((0, 1))((current, next) => Some((current, (next, current+next))) )
    }

    def fibs_2: Stream[Int] = {
        unfold((0, 1)){case (current, next) => Some((current, (next, current+next)))}
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
        case Some((a, s)) => Stream.cons(a, unfold(s)(f))
        case None => Stream.empty
    }
}

object MyLazinessModule {
    def main(args: Array[String]): Unit =
        val s1: Stream[Int] = Stream(1, 2, 3)
        val s2: Stream[Int] = Stream(1, 2)
        println(s1.startsWith(s2))
}