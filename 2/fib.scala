def fib(n:Int):Int = {
	def go(n:Int):Int = n match {
		case 0 | 1 =>  n
		case _ => go(n-1) + go(n-2)
	}
	go(n)
}

def fib3( n : Int) : Int = { 
  def fib_tail( n: Int, a:Int, b:Int): Int = n match {
    case 0 => a 
    case _ => fib_tail( n-1, b, a+b )
  }
  return fib_tail( n, 0, 1)
}

println(fib(10))
println(fib3(10))
