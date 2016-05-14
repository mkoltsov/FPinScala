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
