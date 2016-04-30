sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  /*
  Of course, we can also implement `flatMap` with explicit pattern matching.
  */
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  /*
  Again, we can implement this with explicit pattern matching.
  */
  def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  /*
  This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}


case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

val a = Some(4)

println(a.flatMap(x=>Some(x)))

def mean(xs:Seq[Double]):Option[Double] = if (xs.isEmpty) None else Some(xs.sum/xs.length)
	

def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(x=> mean(xs.map(y=> math.pow(y-x,2))))

println(variance(Seq(0,1,2,3,4,5)))

def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

val absO: Option[Double] => Option[Double] = lift(math.abs)

println(absO(Some(-1)))

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (aa => b map (bb => f(aa, bb)))
println(map2(Some(1), Some(3))(_ + _))
println(map2(None, Some(3))((x,y)=> x.toString + y.toString))

def sequence[A](a: List[Option[A]]): Option[List[A]] =  a match {
  case Nil => Some(Nil)
  case x::xs => x.flatMap(h=> sequence(xs).map(h::_))
}

println(sequence(Some(1)::Some(2)::Nil))

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((el, acc)=> map2(f(el), acc)(_::_) )

def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x=>x)

def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
  es match {
    case Nil => Right(Nil)
    case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
  }

def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
  es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = 
  traverse(es)(x => x)


