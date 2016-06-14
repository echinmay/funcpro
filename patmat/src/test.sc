import patmat.Huffman
import patmat.Huffman.{Fork, Leaf}

object testo {
  val a = "This is a test".toCharArray.toList
  val x = a.groupBy( (c: Char) => c)
  val y = for (
    p <- x
  ) yield(p._1, p._2.size)
  val z = y.toList
  z.sortWith((l: (Char, Int), r: (Char, Int)) => r._2 < l._2)

  val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  val d = 'a' to 'z'
  val e = ('A' to 'Z') ++ (d)
}