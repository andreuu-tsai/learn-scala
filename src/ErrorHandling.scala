sealed trait Option[+A]{
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }
    
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
        case (_, None) => None
        case (None, _) => None
        case (Some(a), Some(b)) => Some(f(a, b))
    }
    
    def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a.flatMap(aa => b.map(bb => f(aa, bb)))

    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
        map(f).getOrElse(None)

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case _ => this 
    }
    
    def orElse_1[B >: A](ob: => Option[B]): Option[B] =
        map(a => Some(a)).getOrElse(ob)
    
    def filter_1(f: A => Boolean): Option[A] = 
        flatMap(a => if(f(a)) then Some(a) else None)
    
    def filter(f: A => Boolean): Option[A] = this match {
        case Some(a) if(f(a)) => Some(a)
        case _ => None
    }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
    def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
        case Nil => None
        case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
    } 

    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
        case Nil => Some(Nil)
        case h :: t => f(h).flatMap( b => traverse(t)(f).map(b :: _))
    }
    
    def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
        traverse(as)(x => x)
}

sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => b
        case Right(a) => Right(a)
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
        for 
            a <- this
            b1 <- b
        yield f(a, b1)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
        case Nil => Right(Nil)
        case h :: t => f(h).flatMap( aa => traverse(t)(f).map(aa :: _))
    }
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
        traverse(es)(x => x)
}


object MyErrorModule {
    def mean(xs: Seq[Double]): Option[Double] =
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)
    
    def variance(xs: Seq[Double]): Option[Double] = {
        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    def main(args: Array[String]): Unit = {
        val l: Seq[Double] = List(1.0, 2.0, 3.0)

        println("variance: %d".format(variance(l).getOrElse("NaN")))
    }
}
