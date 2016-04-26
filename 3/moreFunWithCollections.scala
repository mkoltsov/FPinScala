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

def turnToStr[A](ls:List[A]):List[String] = {
	ls match {
		case Nil => Nil
		case x::xs => x.toString::turnToStr(xs) 
	}
}

println(turnToStr(1.0::2.0::7.91::Nil))

// println(d)