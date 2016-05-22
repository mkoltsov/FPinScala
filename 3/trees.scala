sealed trait Tree[+A]

case class Leaf[A](value:A) extends Tree[A]

case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

def size[A](tr:Tree[A]):Int = tr match {
	case Leaf(_) => 1
	case Branch(l, r) =>  1 + size(l) + size(r) 
}

def max(tr:Tree[Int]):Int ={
	def inner(tr1:Tree[Int], acc:Int):Int = tr1 match {
		case Leaf(v) => acc.max(v)
		case Branch(l, r) => inner(l, acc).max(inner(r,acc))  
	}
	inner(tr, 0)
}

def maxOptimal(tr:Tree[Int]):Int = tr match {
		case Leaf(v) => v
		case Branch(l, r) => maxOptimal(l) max maxOptimal(r)  
}


val b1 = Branch(Leaf("a"), Leaf("b"))
val b2 = Branch(Leaf("c"), Leaf("d"))
val union = Branch(b1, b2)

println(size(union))

val a1 = Branch(Leaf(1), Leaf(11))
val a2 = Branch(Leaf(9), Leaf(10))
val union1 = Branch(a1, a2)

println(max(union1))
println(maxOptimal(union1))

def depth(tr:Tree[Int]):Int ={
	def inner(tr1:Tree[Int], acc:Int):Int = tr1 match {
		case Leaf(v) => acc+1
		case Branch(l, r) => inner(l, acc+1) max(inner(r,acc+1))  
	}
	inner(tr, 0)
}

def depthOptimal(tr:Tree[Int]):Int = tr match {
	case Leaf(_) => 1
	case Branch(l, r) => 1 + depthOptimal(l) max depthOptimal(r)
}

println(depth(union1))
println(depthOptimal(union1))

def map[A,B](tr:Tree[A])(f:A=>B):Tree[B] = tr match {
	case Leaf(v) => Leaf(f(v)) 
	case Branch(l, r) => Branch(map(l)(f), map(r)(f)) 
}

println(map(union1)(_.toString))

def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = {
	case Leaf(v) => l(v)
	case Branch(le, r) => b(fold(le)(l)(b), fold(r)(l)(b)) 
}

