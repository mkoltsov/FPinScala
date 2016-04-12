def tail(l:List[_]):List[_] = l match {
	case _ :: xs => xs
	case Nil => Nil  
}

def setHead[T](l:List[T], head:T) = l match {
	case x::xs => head::xs
	case Nil => Nil 
}


println(tail(1::2::3::Nil))
println(setHead(1::2::3::Nil, 8))
println(Nil)
