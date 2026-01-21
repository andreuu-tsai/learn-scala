package fpinscala.simple

object MySimpleModule{
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = 
        def loop(i: Int): Boolean = {
            if (i == as.length-1) then true
            else if (ordered(as(i), as(i+1))) then false
            else loop(i+1)
        }
        loop(0)

    

    def fib(n: Int): Int = 
        @annotation.tailrec
        def go(n: Int, current:Int, next:Int): Int = {
            if (n<=0) current
            else go(n-1, next, current+next)
        }
        go(n, 0, 1)

    def sorted(i: Int, j: Int): Boolean =
        if (i-j >= 0) then true
        else false

    def partial1[A, B, C](a: A, f: (A, B) => C): B => C=
        (b: B) => f(a, b)

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = 
        (a: A) => (b: B) => f(a, b)

    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
        (a: A, b: B) => f(a)(b)

    def compose[A, B, C](f: B => C, g: A => B): A => C =
        a => f(g(a))

    def main(args: Array[String]): Unit =
        val l: Array[Int] = Array(1, 2, 4, 3, 4)
        println("The array is sorted: %b".format(isSorted(l, sorted)))
}
