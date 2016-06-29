def lines(filename: String): IO[Stream[String]] =
 IO { val src = io.Source.fromFile(filename) src.getLines.toStream append { src.close; Stream.empty }
}