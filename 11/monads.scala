trait Functor[F[_]] {
	def map[A,B](fa: F[A])(f: A => B): F[B]
	def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
	def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] =
	 e match {
		case Left(fa) => map(fa)(Left(_))
		case Right(fb) => map(fb)(Right(_)) 
	}
}

val listFunctor = new Functor[List] {
	def map[A,B](as: List[A])(f: A => B): List[B] = as map f
}

trait Monad[F[_]] extends Functor[F] {
	def unit[A](a: => A): F[A]
	def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
	def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
	def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
}

object Monad {
	val genMonad = new Monad[Gen] {
		def unit[A](a: => A): Gen[A] = Gen.unit(a)
		def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
			ma flatMap f 
		}
}

val parMonad = new Monad[Par] {
  def unit[A](a: => A) = Par.unit(a)
  def flatMap[A,B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
}

def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
  def unit[A](a: => A) = p.succeed(a)
  def flatMap[A,B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
}

val optionMonad = new Monad[Option] {
  def unit[A](a: => A) = Some(a)
  def flatMap[A,B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
}

val streamMonad = new Monad[Stream] {
  def unit[A](a: => A) = Stream(a)
  def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
}

val listMonad = new Monad[List] {
  def unit[A](a: => A) = List(a)
  def flatMap[A,B](ma: List[A])(f: A => List[B]) = ma flatMap f
}

// Since `State` is a binary type constructor, we need to partially apply it
// with the `S` type argument. Thus, it is not just one monad, but an entire
// family of monads, one for each type `S`. One solution is to create a class
// `StateMonads` that accepts the `S` type argument and then has a _type member_
// for the fully applied `State[S, A]` type inside:
class StateMonads[S] {
  type StateS[A] = State[S, A]

  // We can then declare the monad for the `StateS` type constructor:
  val monad = new Monad[StateS] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}

// But we don't have to create a full class like `StateMonads`. We can create
// an anonymous class inline, inside parentheses, and project out its type member `f`.
// This is sometimes called a "type lambda", since it's very similar to a type-level
// anonymous function.
def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
  def unit[A](a: => A): State[S, A] = State(s => (a, s))
  override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
    st flatMap f
}

def sequence[A](lma: List[F[A]]): F[List[A]] =
  lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
  la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

/** 
 * 'Balanced' sequencing, which should behave like `sequence`,
 * but it can use less stack for some data types. We'll see later
 * in this chapter how the monad _laws_ let us conclude both 
 * definitions 'mean' the same thing.
 */
def bsequence[A](ms: Seq[F[A]]): F[IndexedSeq[A]] = {
  if (ms.isEmpty) point(Vector())
  else if (ms.size == 1) ms.head.map(Vector(_))
  else {
    val (l,r) = ms.toIndexedSeq.splitAt(ms.length / 2)
    map2(bsequence(l), bsequence(r))(_ ++ _)
  }
}

// Recursive version:
def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
  if (n <= 0) unit(List[A]()) else map2(ma, replicateM(n - 1, ma))(_ :: _)

// Using `sequence` and the `List.fill` function of the standard library:
def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
  sequence(List.fill(n)(ma))


/*
For `Par`, `filterM` filters a list, applying the functions in
parallel; for `Option`, it filters a list, but allows
the filtering function to fail and abort the filter
computation; for `Gen`, it produces a generator for 
subsets of the input list, where the function `f` picks a 
'weight' for each element (in the form of a
`Gen[Boolean]`)
*/
def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
  ms match {
    case Nil => unit(Nil)
    case h :: t => flatMap(f(h))(b =>
      if (!b) filterM(t)(f)
      else map(filterM(t)(f))(h :: _))
  }

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
  a => flatMap(f(a))(g)

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
  compose((_:Unit) => ma, f)(())


  case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A,B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
  }
}

case class State[S, A](run: S => (A, S)) {
	 def map[B](f: A => B): State[S, B] =
		State(s => {
			val (a, s1) = run(s) (f(a), s1)
		})
	def flatMap[B](f: A => State[S, B]): State[S, B] =
		State(s => {
			val (a, s1) = run(s)
	    	  f(a).run(s1)
    	})
}

type IntState[A] = State[Int, A]

object IntStateMonad extends Monad[IntState] {
	def unit[A](a: => A): IntState[A] = State(s => (a, s))
	def flatMap[A,B](st: IntState[A])(f: A => IntState[B]): IntState[B] = st flatMap f 
}

object IntStateMonad extends
	Monad[({type IntState[A] = State[Int, A]})#IntState] { ...
}

def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
	def unit[A](a: => A): State[S,A] = State(s => (a, s))
	def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] = st flatMap f 
}

/ Getting and setting the same state does nothing:
getState.flatMap(setState) == unit(())

// written as for-comprehension:
for {
  x <- getState
  _ <- setState(x)
} yield ()

// Setting the state to `s` and getting it back out yields `s`.
setState(s).flatMap(_ => getState) == unit(s)

// alternatively:
for {
  _ <- setState(s)
  x <- getState
} yield x


val F = stateMonad[Int]
def zipWithIndex[A](as: List[A]): List[(Int,A)] = as.foldLeft(F.unit(List[(Int, A)]()))((acc,a) =>
 for {
	xs <- acc
            n  <- getState
            _  <- setState(n + 1)
	} yield (n, a) :: xs).run(0)._1.reverse

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader(r => f(st.run(r)).run(r))
  }

  // A primitive operation for it would be simply to ask for the `R` argument:
  def ask[R]: Reader[R, R] = Reader(r => r)
}

// The action of Reader's `flatMap` is to pass the `r` argument along to both the
// outer Reader and also to the result of `f`, the inner Reader. Similar to how
// `State` passes along a state, except that in `Reader` the "state" is read-only.

// The meaning of `sequence` here is that if you have a list of functions, you can
// turn it into a function that takes one argument and passes it to all the functions
// in the list, returning a list of the results.

// The meaning of `join` is simply to pass the same value as both arguments to a
// binary function.

// The meaning of `replicateM` is to apply the same function a number of times to
// the same argument, returning a list of the results. Note that if this function
// is _pure_, (which it should be), this can be exploited by only applying the
// function once and replicating the result instead of calling the function many times.
// This means the Reader monad can override replicateM to provide a very efficient
// implementation.