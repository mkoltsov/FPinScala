trait IO { self =>
	def run: Unit
	def ++(io: IO): IO = new IO {
   	def run = { self.run; io.run }
   } 
}

object IO {
	def empty: IO = new IO { def run = () }
}

sealed trait IO[A] { self =>
 def run: A
 def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }
 def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run } 
}

object IO extends Monad[IO] {
	def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
	def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f 
	def apply[A](a: => A): IO[A] = unit(a)
}

def ReadLine: IO[String] = IO { readLine }
def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

def converter: IO[Unit] = for {
_ <- PrintLine("Enter a temperature in degrees Fahrenheit: ") d <- ReadLine.map(_.toDouble)
_ <- PrintLine(fahrenheitToCelsius(d).toString)
} yield ()

sealed trait IO[A] {
def flatMap[B](f: A => IO[B]): IO[B] =
FlatMap(this, f)
def map[B](f: A => B): IO[B] =
    flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

@annotation.tailrec def run[A](io: IO[A]): A = io match { case Return(a) => a
case Suspend(r) => r()
case FlatMap(x, f) => x match {
    case Return(a) => run(f(a))
case Suspend(r) => run(f(r()))
case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
}
}

sealed trait TailRec[A] {
def flatMap[B](f: A => TailRec[B]): TailRec[B] =
FlatMap(this, f)
def map[B](f: A => B): TailRec[B] =
    flatMap(f andThen (Return(_)))
}
case class Return[A](a: A) extends TailRec[A]
case class Suspend[A](resume: () => A) extends TailRec[A] case class FlatMap[A,B](sub: TailRec[A],
k: A => TailRec[B]) extends TailRec[B]

sealed trait Free[F[_], A]
final case class Point[F[_], A](a: A) extends Free[F, A]
final case class Join[F[_], A](s: F[Free[F, A]]) extends Free[F, A]