import forcomp.Anagrams._
import forcomp.Anagrams.Sentence

object senten {

  /*
   *
   * Dont call with empty thisword
   */
  def mergesubsetlists(thisword: Word, lists: List[List[Word]]): List[List[Word]] = {
    if (lists isEmpty) {
      List(List(thisword))
    } else {
      for {
        sublists <- lists
      } yield thisword :: sublists
    }
  }

  val sublist1 = List(List("a"), List("b"))
  val addc = mergesubsetlists("c", sublist1)
  val addctoempty = mergesubsetlists("c", List())

  /*
   *
   * 1. Start with a sentence.
   * 2. Create an occurence from the sentence.
   * 3. The anagrams of the sentence is the permutation and combination of this occurence.
   *
   *    def getanagramsfromoccurence(occ: Occurences): List[Word]
   *
   *    Now given an occurence take all the combinations of the occurence.
   *    Of all possible combinations start with the list of occurences that has a word associated
   *    with it or in other words eliminate those combinations that dont result in a word.
   *
   *    For each occurence what we have to do is to subtract this occurence from the original
   *    occurence. Repeat this combinations on this smaller subset. The subsets will return a List
   *    of List of words. Add the current word to all the lists returned and return the new list
   *
   *    def getlistofwords(): List[List[Words]]
   */
  def getlistofwords(occ: Occurrences): List[List[Word]] = {
    val combos = combinations(occ)
    val validcombos = combos.filter((o) => dictionaryByOccurrences contains(o))
    if (validcombos.size == 0) List() else {
      val alllists = for {
        c <- validcombos
        w <- dictionaryByOccurrences(c)
        if (w.size != 0)
      } yield mergesubsetlists(w, getlistofwords(subtract(occ, c)))
      alllists.flatten
    }
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val soccur = sentenceOccurrences(sentence)
    getlistofwords(soccur)
  }
}