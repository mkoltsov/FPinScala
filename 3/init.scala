def init[A](l: List[A]): List[A] = {
	l.flatMap {
		case a:Int if a == l(l.size-1) => Nil
		case other 					   => List(other)
	}
}

println(init(List(1,2,3,4,5)))