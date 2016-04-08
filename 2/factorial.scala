def factorial(n:Int):Int = {
	@annotation.tailrec
	def go(n:Int, acc:Int):Int = 
		if (n<=0) acc
		else {
			println(acc)
			go(n-1, n*acc)
		}
	go(n, 1)
}

println(factorial(4))