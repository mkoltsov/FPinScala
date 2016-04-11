def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
	(a:A) => (b:B) => f(a,b) 
}

def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
	(a:A, b:B) => f(a)(b)
}

def compose[A,B,C](f: B => C, g: A => B): A => C = {
	(a:A) => f(g(a))
}

def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)

def line(a:Int, b:Int,x:Int) = a*b*x

def lineCurried(a:Int)(b:Int)(x:Int)=line(a,b,x)

val ch = (line _).curried

val p1 = ch(1)(2)
//val p2 = p1(3)(4)
println(p1(3))

def partialLine = (x:Int, b:Int) => line(2,x,b)

def partialLIne2 = line(2, _:Int, _:Int)

