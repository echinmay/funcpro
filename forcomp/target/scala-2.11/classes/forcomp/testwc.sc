import forcomp.Anagrams
import forcomp.Anagrams.Occurrences

object testwc {
  val x = Anagrams.wordOccurrences("hih")
  val y = Anagrams.wordOccurrences("abc")

  def getnewocc(c: Char, n: Int): Anagrams.Occurrences = {
    (for {
      a <- (1 to n)
    } yield (c, a)).toList
  }

  def mergeocc(o1: Anagrams.Occurrences, o2: Anagrams.Occurrences): List[Anagrams.Occurrences] = {
    for {
      a1 <- o1
      a2 <- o2
    } yield List(a1, a2)
  }

  def testp(o1: Anagrams.Occurrences, o2: Anagrams.Occurrences): Unit = {
    for (
      a <- o1;
      b <- o2
    ) yield(print (" ==>" + a +  b + "\n"))
  }

  def makelist(c: Char, n: Int) : List[Occurrences] = {
    val l = for {
      n <- 1 to n
    } yield (List((c, n)))
   // println(" ==> ", l.toList)
    l.toList
  }

  def mergeList(left: List[Anagrams.Occurrences], right: List[Anagrams.Occurrences]): List[Anagrams.Occurrences] =
  {
    if (right.size == 0) left else if (left.size == 0) right else {
      val allpairs = for {
        a <- left;
        b <- right
      } yield (a ++ b)
      println(allpairs)
      allpairs
    }
  }

  def getcombonew(o: Anagrams.Occurrences): List[Anagrams.Occurrences] = {
    val z = for {
      anag <- o
    } yield(makelist(anag._1, anag._2))
    z.flatten.toList
  }

  val g1 = getcombonew(List(('a', 2), ('b', 2)))
  val t1 = makelist('a', 2)
  val t2 = makelist('b', 2)

  def mergeleaf(l: (Char, Int), r: (Char, Int)): List[Anagrams.Occurrences] = {
    mergeList(makelist(l._1, l._2), makelist(r._1, r._2))
  }

  val t1t2 = mergeleaf(('a', 2), ('b',2))
  def getallcombos2(grams: Anagrams.Occurrences): List[Anagrams.Occurrences] = {
    def getallcomboshlpr(gramsiter: Anagrams.Occurrences, acc: List[Anagrams.Occurrences]): List[Anagrams.Occurrences] = {
      gramsiter match {
        case Nil => acc
        case y :: Nil => makelist(y._1, y._2) ++ acc
        case x :: xs => {
          val left = makelist(x._1, x._2)
          println("getallcombos hlpr", left)
          mergeList(left , getallcomboshlpr(xs, left ++ acc ))
        }
      }
    }
    getallcomboshlpr(grams, List())
  }

  val testcombo = List(('a', 2), ('b', 2))
  val res = getallcombos2(testcombo)
 // res
}
