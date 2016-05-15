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
