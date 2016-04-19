def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
	case Nil => z
	case x::xs => f(x,foldRight(xs,z)(f)) 
}

println(foldRight(1::2::3::4::Nil, 0)(_+_))
println(foldRight(1::2::3::4::Nil, 1)(_*_))