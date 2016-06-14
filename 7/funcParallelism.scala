def sum(ints: IndexedSeq[Int]): Int = 
if (ints.size <= 1)
ints.headOption getOrElse 0
 else {
	val (l,r) = ints.splitAt(ints.length/2) sum(l) + sum(r)
}

def sum(ints: IndexedSeq[Int]): Par[Int] = if (ints.size <= 1)
Par.unit(ints.headOption getOrElse 0) else {
val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(sum(l), sum(r))(_ + _)
  }

 def map2[A,B](f1: =>B, f2: =>B):B =  