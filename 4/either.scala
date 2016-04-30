sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

trait Either[+E, +A] {
def map[B](f: A => B): Either[E, B] = this match {
	case Right(v) => Right(f(v))
	case other =>  other
}
  
def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
  this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
  this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): 
  Either[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)
// def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
// def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] 
// def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] 

def traverse[E, A, B](as: List[A])(
                      f: A => Either[E, B]): Either[E, List[B]] = as.foldRight()
