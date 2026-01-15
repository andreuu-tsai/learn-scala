sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object MyTreeModule {

    def size[A](tree: Tree[A]): Int = tree match {
        case Branch(l, r) => 1 + size(l) + size(r)
        case Leaf(_) => 1
    }

    def maximun(tree: Tree[Int]): Int = tree match {
        case Branch(l, r) => maximun(l) max maximun(r)
        case Leaf(v) => v
    }

    def depth[A](tree: Tree[A]): Int ={
        def go[A](tree: Tree[A], carry: Int): Tree[Int] = tree match {
            case Branch(l, r) => Branch(go(l, carry+1), go(r, carry+1))
            case Leaf(v) => Leaf(carry)
        }
        maximun(go(tree, 0))
    }
    
    def depth2[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 0
        case Branch(l, r) => 1 + (depth2(l) max depth2(r))
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(a) => Leaf(f(a))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
        case Leaf(a) => f(a)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def main(args: Array[String]): Unit = {
        val tree = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Branch(Leaf(8), Leaf(3)))

        println("Size: %d".format(size(tree)))
        println("Depth: %d".format(depth(tree)))
        println("Depth: %d".format(depth2(tree)))
        println("Maximun: %d".format(maximun(tree)))
    }
}