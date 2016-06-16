import forcomp.Anagrams
import forcomp.Anagrams.Occurrences

object testfresh {

  def try2(o: Anagrams.Occurrences): List[Anagrams.Occurrences] = {
    val z = for {
      elem <- o
      n <- 1 to elem._2
    } yield (List((elem._1, n)))
    z.toList
  }

  val l = List(('a', 2), ('b', 2))
  val trytest = try2(l)

  def genlist(c: Char, n: Int): List[Anagrams.Occurrences] = {
    val z = for {
      n <- 1 to n
    } yield (List((c, n)))
    z.toList
  }

  val z = l.map(x => genlist(x._1, x._2))

  def addelem(elem: (Char, Int), occList: List[Anagrams.Occurrences]): List[Anagrams.Occurrences] = {
    val combos = genlist(elem._1, elem._2)
      val occListPlus = for {
        occ <- occList
        newe <- combos
      } yield (occ ++ newe)
      combos ::: occListPlus
  }

  val first = addelem(('a', 2), List())
  val addsecond = addelem(('b', 2), genlist('a', 2))

  def combitry(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List()
    case (c, i)::Nil => {
      val lastelem = genlist(c, i)
      println("()()()" + lastelem)
      lastelem
    }
    case (c, i)::rest => {
      val restcombo = combitry(rest)
      println(" ===" + restcombo)
      addelem((c, i), combitry(rest)) ::: restcombo
    }
  }
  val testlist = List(('a', 2), ('b', 2), ('c', 2))
  //val testlist = List(('a', 2), ('b', 2))
  val trytestlist = combitry(testlist)
}

