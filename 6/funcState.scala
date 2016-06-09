trait RNG {
	def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG { def nextInt: (Int, RNG) = { 
	val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
	val nextRNG = SimpleRNG(newSeed)
	val n = (newSeed >>> 16).toInt
	(n, nextRNG)
	}
}

def nonNegativeInt(rng: RNG): (Int, RNG) = {
	val (s, n) = rng.nextInt
	(if (i < 0) -(i + 1) else i, r)
}

def double(rng: RNG): (Double, RNG) = {
  val (i, r) = nonNegativeInt(rng)
  (i / (Int.MaxValue.toDouble + 1), r)
}
