def addOne(ls:List[Int]):List[Int] = {
	def go(ls:List[Int], acc:List[Int]):List[Int] =
	ls match {
		case Nil => acc
		case x::xs => x+1::go(xs, acc) 
	}
  go(ls, Nil)
}


println(addOne(1::2::4::Nil))

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
	case Nil  => z
	case x::xs => f(x,foldRight(xs,z)(f)) 
}

println(foldRight(List(1,2,4), Nil:List[Int])((x, acc) => x+1::acc))

def turnToStr(ls:List[Double]):List[Double] = {
	def go(ls:List[Double], acc:List[String]):List[Double] =
	ls match {
		case Nil => acc
		case x::xs => x.toString::go(xs, acc) 
	}
  go(ls, Nil)
}

println(turnToStr(1.0::2.0::7.91::Nil))