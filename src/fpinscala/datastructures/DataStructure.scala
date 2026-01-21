package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def tail[A](l: List[A]): List[A] = l match
        case Nil => sys.error("tail of empty list")
        case Cons(_, t) => t
    
    
    def setHead[A](h: A, l: List[A]): List[A] = l match 
        case Nil => sys.error("setHead on empty list")
        case Cons(_, t) => Cons(h, t)

    def drop[A](l: List[A], n: Int): List[A] = {
        if n <= 0 then l
        else l match
            case Nil => Nil
            case Cons(_, t) => t
    }
    
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
        l match 
            case Cons(h, t) if f(h) => dropWhile(t, f)
            case _ => l 
    }

    def init[A](l: List[A]): List[A] = {
        l match 
            case Nil => sys.error("init of empty list")
            case Cons(head, Nil) => Nil
            case Cons(head, tail) => Cons(head, init(tail))
    }

    def apply[A](as: A*): List[A] = 
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail*))
}

object MyModule {
    def foldRight[A, B](as: List[A], z: B)(f:(A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
    
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }

    def sum(ns: List[Int]): Int = {
        foldRight(ns, 0)((x, y)=> x + y)
    }
    
    def leftSum(ns: List[Int]): Int = {
        foldLeft(ns, 0)((acc, x) => acc + x)
    }

    def product(ns: List[Double]): Double = {
        foldRight(ns, 1.0)((x, y) => x * y)
    }

    def length[A](as: List[A]): Int = {
        foldRight(as, 0)((_, acc) => acc + 1)
    }

    def reverse[A](as: List[A]): List[A] = {
        foldLeft(as, Nil)((acc, x) => Cons(x, acc))
    }

    def appendr[A](l: List[A], r: List[A]): List[A] = {
        foldRight(l, r)((x, xs) => Cons(x, xs))
    }

    def appendl[A](l: List[A], r: List[A]): List[A] =
        foldLeft(reverse(l), r)((acc, x) => Cons(x, acc))

    def concatenate[A](l: List[List[A]]): List[A] = 
        foldRight(l, Nil)((x, y) => appendr(x, y))

    def addOne(l: List[Int]): List[Int] =
        foldRight(l, Nil)((h, t) => Cons(h+1, t))
    
    def doubleToString(l: List[Double]): List[String] =
        foldRight(l, Nil)((h, t)=> Cons(h.toString(), t))

    def map[A, B](as: List[A])(f: A => B): List[B] = 
        foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

    def filter[A](l: List[A])(f: A => Boolean): List[A] =
        foldRight(l, Nil)((h, t) => if f(h) then Cons(h, t) else t)

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
        concatenate(map(l)(f))

    def filter2[A](l: List[A])(f: A => Boolean): List[A] =
        flatMap(l)(a => if (f(a)) then List(a) else Nil)
    
    def addPairwise(l: List[Int], r: List[Int]): List[Int] = (l, r) match
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addPairwise(t1, t2))
    
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))

    @annotation.tailrec
    def startWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
        case (_, Nil) => true
        case (Cons(h, t), Cons(h2, t2)) if h == h2 => startWith(t, t2)
        case _ => false
    }

    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
        case Nil => sub == Nil
        case _ if startWith(sup, sub) => true
        case Cons(_, t) => hasSubsequence(t, sub) 
    }

    def main(args: Array[String]): Unit={
        val l = List(1, 2, 3, 5)
        val l2 = List(1.0, 2.0, 3.0, 5.0)
        val l3 = List(6, 7, 8)
        val l4 = List(10, 11, 12)
        val l5 = List(l, l3, l4)
        
        println(length(l))
        println(sum(l))
        println(reverse(l))
        println(leftSum(l))
        println(product(l2))
        println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
        println(appendr(l, l3))
        println(appendl(l, l3))
        println(concatenate(l5))
        println(addOne(l))
        println(doubleToString(l2))
        println(filter2(l)( i => i % 2 == 0))
        println(flatMap(l)(a => List(a, a)))
        println(addPairwise(l3, l4))
    }
}