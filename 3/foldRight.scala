def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
	case Nil  => z
	case x::xs => f(x,foldRight(xs,z)(f)) 
}

def foldRightSC[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
	case Nil  => z
	case x::xs if x==0.0 => z
	case x::xs => f(x,foldRight(xs,z)(f)) 
}

println(foldRight(1::2::3::4::Nil, 0)(_+_))
println(foldRight(1.0::2.0::3.0::4.0::Nil, 1.0)(_*_))
println(foldRight(1.0::2.0::3.0::4.0::Nil, Nil:List[Double])(_::_))

def length[A](as: List[A]): Int = {
	foldRight(as, 0)((_,acc) => acc + 1)
}

println(length(1::2::3::4::Nil))
//cause foldRight is not tail-recursive aka not stack safe this results in StackOverflow
// foldRight((0 to 10000).toList, 0)(_-_)
@annotation.tailrec
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = 
	as match {
		case Nil =>  z
		case x::xs => foldLeft(xs, f(z,x))(f)	
	}


println(foldLeft(1::2::3::4::Nil, 0)(_+_))
println(foldLeft(1::2::3::4::Nil, 1)(_*_))
println(foldLeft(1::2::3::4::Nil, 0)((_,acc) => acc + 1))

@annotation.tailrec
def reverse[A](ls:List[A], acc:List[A]):List[A] =
ls match {
	case Nil => acc
	case x::xs => reverse(xs, x::acc)
}

println(reverse(List(1,2,3,4), Nil))

println(foldLeft(1.0::2.0::3.0::4.0::Nil, Nil:List[Double])((acc,h) => h::acc))

foldLeft((0 to 100000).toList, 0)(_-_)

def append[A](ls:List[A], elem:A):List[A] = foldRight(ls, elem::Nil)(_::_)

println(append(List(1,2,3,4), 5))

def concat[A](ls:List[List[A]]):List[A] = foldRight(ls, Nil:List[A]){case (x, acc) => x:::acc}

println(concat(List(List(1,2,3,4), List(5,6,74))))