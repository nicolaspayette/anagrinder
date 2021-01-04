object Anagrinder {

  val dict = scala.io.Source.fromFile("/usr/share/dict/british-english").getLines.toSet

  def anagrams(s: String): Set[String] =
    (for {
      permSeq <- s.toSeq.permutations
      perm = permSeq.toString
      if dict.contains(perm)
    } yield perm).toSet

  def subSequences(s: String): Set[String] =
    (for {
      i <- 0 until s.length
      j <- i + 1 to s.length
      ss = s.subSequence(i, j)
      if ss.length > 1
    } yield ss.toString).toSet

  def subAnagrams(s: String): Map[String, Set[String]] =
    (for {
      ss <- subSequences(s)
      as = anagrams(ss)
      if as.nonEmpty
    } yield ss -> as).toMap

  def prettyPrint(m: Map[String, Iterable[String]]): Unit = {
    if (m.nonEmpty) {
      val offset = m.keys.map(_.length).max + 2
      for ((k, vs) <- m.toSeq.sortBy(_._1.length)) {
        println(k + ":\n" + (" " * offset) + vs.mkString(" "))
      }
    }
  }

  def a(s: String) = prettyPrint(subAnagrams(s))
}
