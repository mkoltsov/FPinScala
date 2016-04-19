
def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
case Cons(h,t) if f(h) => dropWhile(t)(f)
case _ => as }