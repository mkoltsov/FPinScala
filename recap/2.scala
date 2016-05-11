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
	def go[A](ls:List[A], acc:List[A]):List[A] = {
		ls match {
			case Nil => Nil
			case x::Nil => acc.reverse
			case x::xs => go(xs, x::acc)
		}
	}

	go(l, Nil:List[A])
}

println(init(1::2::3::5::Nil))
