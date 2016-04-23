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

def map[A,B](as: List[A])(f: A => B): List[B] = 
	foldRight(as, Nil:List[B])((h,t) => f(h)::t)


println(map(1.0::2.0::7.91::Nil)(_.toString))
// println(d)

def filter[A](as: List[A])(f: A => Boolean): List[A] = {
	foldRight(as, Nil:List[A])((h,t) => if (f(h)) h::t else t)
}

println(filter(1.0::2.0::7.91::Nil)(_%2==0))

def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
	foldRight(as, Nil:List[B])((h,t)=>f(h):::t)
}

println(flatMap(List(1,2,3))(i => List(i,i)))

def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
	flatMap(as)(i=> if(f(i)) List(i) else Nil)
}

println(filterViaFlatMap(1.0::2.0::7.91::Nil)(_%2==0))

def zipWith[A, B](la:List[A], lb:List[b])(f:(A, A) => B)= 
