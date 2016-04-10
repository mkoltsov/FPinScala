def findFirst[A](as:Array[A], f:A=>Boolean):Int = {
		@annotation.tailrec
		def loop(n:Int):Int = {
			if (n>=as.length) -1
			else if (f(as(n))) n 
			else loop(n+1)
		}
		loop(0)
}

println(findFirst(Array("chef","pupa", "dupa"), (a:String)=> a=="dupa"))