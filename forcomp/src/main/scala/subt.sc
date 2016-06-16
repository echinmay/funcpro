import forcomp.Anagrams
import forcomp.Anagrams.Occurrences

object subt {


  def subtracttest(x: Occurrences, y: Occurrences): Occurrences = {
    val xmap = x.groupBy( (c) => c._1)
    def getelem(needle: (Char, Int), m: Map[Char, List[(Char, Int)]]): (Char, Int) = m(needle._1).head

    def updatemap(e: (Char, Int), m: Map[Char, List[(Char, Int)]]): Map[Char, List[(Char, Int)]] = {
      val found = getelem(e, m)
      if (found._2 > e._2) {
        val newnum = found._2 - e._2
        m updated(e._1, List((e._1, newnum)))
      } else {
        m - e._1
      }
    }

    val updatedmap = y.foldLeft(xmap)( (m, e) => updatemap(e, m))
    val newlist = for {
      v <- updatedmap.values
    } yield (v.head)
    newlist.toList.sortWith((l, r) => l._1 < r._1)
  }

  val occ1 = Anagrams.wordOccurrences("Rooobert")
  val occ2 = Anagrams.wordOccurrences("ot")
  val s = subtracttest(occ1, occ2)
}