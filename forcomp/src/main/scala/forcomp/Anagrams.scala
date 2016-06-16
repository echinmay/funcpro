package forcomp

import scala.collection.JavaConverters._


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    val o = (for (g <- w.toList.groupBy(c => c.toLower)) yield (g._1, g._2.size)).toList
    o.sortWith((l, r) => l._1 < r._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    val squash = s.fold("")(_ + _)
    wordOccurrences(squash)
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =  dictionary.groupBy(wordOccurrences(_))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /* Given a char and an int generate all List of occurences for this pair.
   * For example a ('a', 2) will Generate a
   * List(
   * List('a', 1)
   * List('a', 2)
   * )
   */
  def genlist(c: Char, n: Int): List[Anagrams.Occurrences] = {
    val z = for {
      n <- 1 to n
    } yield (List((c, n)))
    z.toList
  }

  /*
   * Add an element to an exsting list. This first generates a list for the new element.
   * Then it goes through the current occurences and adds the new elements to it.
   */
  def addelem(elem: (Char, Int), occList: List[Anagrams.Occurrences]): List[Anagrams.Occurrences] = {
    val combos = genlist(elem._1, elem._2)
    val occListPlus = for {
      occ <- occList
      newe <- combos
    } yield (occ ++ newe)
    combos ::: occListPlus
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    // Fix this to return Occurences in the correct alphabetical order
    def combinations2(occurrences: Occurrences): List[Occurrences] = occurrences match {
      case Nil => List(List())
      case (c, i) :: Nil => genlist(c, i) ::: List(List())
      case (c, i) :: rest => {
        val restcombo = combinations(rest)
        addelem((c, i), combinations(rest)) ::: restcombo
      }
    }

    combinations2(occurrences) map (x => x.sortWith((l, r) => l._1 < r._1))
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
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

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
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
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if (sentence.isEmpty) List(Nil)
    else {
      val soccur = sentenceOccurrences(sentence)
      val allpossiblecombos = getlistofwords(soccur)
      // Filter out those combiations that do not use all the words.
      allpossiblecombos.filter((p) => sentenceOccurrences(p) == soccur)
    }
  }
}
