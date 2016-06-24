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