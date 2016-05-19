def fib(n: Int): Int = {
	@annotation.tailrec
	def go(prePre:Int, Pre:Int, i:Int):Int = i match {
		case `n` => Pre
		case _ => 
		go(Pre, prePre + Pre, i+1)
	}

	go(0, 1, 2)
}

// println(fib(5))
 (3 to 10).foreach(x=> println(s"${fib(x)}"))

def isSorted[A](as: Array[A])(ordered: (A,A) => Boolean): Boolean = {
	@annotation.tailrec
	def loop(n:Int):Boolean = 
		if (n>=as.length-1) true
		else if (!ordered(as(n), as(n+1))) false
		else loop(n+1)
	loop(0)
}

println(isSorted(Array(1,2,3,4))((x,y) => x<y))

def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
(a) => (b) => f(a,b)

def uncurry[A,B,C](f: A => B => C): (A, B) => C = 
(a, b)=> f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C
= (a:A) => f(g(a))

def tail[A](ls:List[A]):List[A] = ls match {
	case Nil => Nil
	case x::xs =>xs 
}

println(tail(1::2::3::4::Nil))

def setHead[A](ls:List[A], head:A):List[A] = ls match {
	case Nil => Nil
	case x::xs =>head::xs 
}

println(setHead(1::2::3::4::Nil, 100))

def drop[A](l: List[A], n: Int): List[A] = l match {
	case Nil   => Nil
	case x::xs if n==0 => x::xs
	case x::xs => drop(xs, n-1)
}

println(drop(1::2::3::4::5::Nil, 2))

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
	case Nil => Nil
	case x::xs if f(x) => dropWhile(xs, f)
	case x::xs if !f(x) => x::xs
}

println(dropWhile(2::4::6::4::5::Nil, (x:Int)=>x%2==0 ))

def init[A](l: List[A]): List[A] = {
	@annotation.tailrec
	def go[A](ls:List[A], acc:List[A]):List[A] = {
		ls match {
			case Nil => Nil
			case x::Nil => acc.reverse
			case x::xs => go(xs, x::acc)
		}
	}

	go(l, Nil:List[A])
}

def init2[A](ls: List[A]): List[A] = ls match {
	case Nil => sys.error("ERROR")
	case (_::Nil) => Nil
	case x::xs => x::init2(xs) 
}


println(init(1::2::3::5::Nil))
println(init2(1::2::3::5::Nil))

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
	case Nil => z
	case x::xs => f(x, foldRight(xs,z)(f))
}

println(foldRight(1::2::3::4::5::Nil, 1)(_*_))
println(foldRight(1::2::3::4::5::Nil, 0)(_+_))

def foldRightExit(as: List[Int], z: Int)(f: (Int, Int) => Int): Int = as match {
	case Nil => z
	case x::xs if x!=0 => z
	case x::xs => f(x, foldRight(xs,z)(f))
}

println(foldRightExit(1::2::3::0::5::Nil, 1)(_*_))

println(foldRight(List(1,2,3), Nil:List[Int])(_::_))

def length[A](as: List[A]): Int = foldRight(as, 0)((_ , acc)=>acc+1)

println(length(1::2::3::4::5::6::7::Nil))

def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
	case Nil => z
	case x::xs => foldLeft(xs, f(z,x))(f)
}

println(foldLeft(1::2::3::4::5::Nil, 0)(_+_))
println(foldLeft(1::2::3::4::5::Nil, 1)(_*_))
println(foldLeft(1::2::3::4::5::Nil, 0)((acc, _)=> acc+1))
println(foldLeft(1::2::3::4::5::Nil, Nil:List[Int])((acc, x) => x::acc ))
def append[A](ls:List[A], elem:A):List[A] = foldRight(ls, elem::Nil:List[A])(_::_)

println(append(1::2::3::Nil, 6))

def flatten[A](list:List[List[A]]):List[A] = foldRight(list, Nil:List[A])((_:::_))
// def flatten[A](list:List[List[A]]):List[A] = foldRight(list, Nil:List[A])(append)

def map[A,B](as: List[A])(f: A => B): List[B] = as match {
	case Nil => Nil
	case x::xs => f(x)::map(xs)(f)
}

println(map(1::2::3::Nil)(_+1))

println(flatten(List(1,2,3)::List(3,2,1)::List(6,2,7)::Nil))

def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
	case Nil => Nil
	case x::xs => if (f(x)) filter(xs)(f) else x::filter(xs)(f)
}
val d  = filter(List(1,2,3))(_%2==0)
println(d)

def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
	case Nil => Nil
	case x::xs => f(x):::flatMap(xs)(f)
}

println(flatMap(List(1,2,3))(i =>List(i,i)))

def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) Nil else List(x))
println(filterViaFlatMap(List(1,2,3))(_%2==0))

def zipWith[A,B](lsA:List[A], lsB:List[A])(f:(A,A)=>B):List[B] = (lsA, lsB) match {
	case (_, Nil) => Nil
	case (Nil, _) => Nil
	case (x::xs, y::ys) => f(x, y)::zipWith(xs, ys)(f) 
}


println(zipWith(1::2::3::Nil, 3::4::5::Nil)(_+_))

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
{
	 
	 (sup, sub) match {
		case (Nil,_) => false 
		case (_, Nil) => false 
		case (x::xs, y::ys) => if (x==y) hasSubsequence(xs, ys)
		case (x::xs, y::Nil) => if (x==y) true 
	}
}

