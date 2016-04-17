def init[A](l: List[A]): List[A] = {
	l.flatMap {
		case a:Int if a == l(l.size-1) => Nil
		case other 					   => List(other)
	}
}

println(init(List(1,2,3,4,5)))

def init1[A](l: List[A]): List[A] = {
	def go(n:Int, acc:List[A]):List[A] = {
		if (n==l.size-1) acc.reverse
		else go(n+1, l(n)::acc)
	}
	go(0, Nil)
}

println(init1(List(1,2,3,4,5)))