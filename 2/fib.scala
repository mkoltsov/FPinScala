def fib(n:Int):Int = {
	def go(i:Int, acc:Int):Int = {
		if (i==n) acc 
		else go(i,0) + go(i+1,0)
	}

	go(0, 0)
}

println(fib(1))
