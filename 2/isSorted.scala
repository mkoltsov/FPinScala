def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
	def loop(n:Int):Boolean = {
		if (n==as.length-1) true
		else {
			if (ordered(as(n), as(n+1))) loop(n+1)
			else false
		}
	}
	loop(0)
}

println(isSorted(Array[Int](1,2,3,4,5), (a:Int, b:Int) => if (a < b) true else false))