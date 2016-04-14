def drop[A](l: List[A], n: Int): List[A] = {
	def go(list:List[A], i:Int):List[A] = {
		if (i==0) list 
		else {
			list match {
				case h::t => go(t, i-1)
				case Nil => Nil 
			}
		}		
	}
	go(l, n)
}

println(drop(1::2::3::4::5::Nil, 4))

def dropWhile(l: List[Int], f: Int => Boolean): List[Int] = 
			l match {
				case h::t if f(h) =>  dropWhile(t,f) 
				case _ => l
			}


println(dropWhile(1::2::1::1::5::Nil, (i:Int) => i==1))