import forcomp.Anagrams.{Occurrences, _}

val combinesLR = combinations(sentenceOccurrences(List("Linux", "rulez")))

combinesLR.map(occurences => subtract(occurences, List(('e', 1), ('r', 1), ('x', 1))))
  .map(occurences => subtract(occurences, List(('i', 1), ('l', 1), ('n', 1))))
  .map(occurences => subtract(occurences, List(('z', 1), ('l', 1), ('u', 2))))
  .filter(occurences => occurences.nonEmpty)


/*
def iterateCombination(combination: List[Occurrences], acc: List[Sentence]): List[Sentence] = combination match {
  case List() => acc
  case occur :: tail =>
    val words = dictionaryByOccurrences.get(occur).get
    acc.flatMap(sentence => words.map(word => sentence :+ word))
    val other = iterateCombination(tail.map(o => subtract(o, occur)).filter(o => o.nonEmpty), acc)
    other ::: ()
}
*/


/*
def iterateCombination(combination: List[Occurrences], acc: List[Sentence], current: Occurrences): List[Sentence] = combination match {
  case List() => acc
  case _ :: tail =>
    val newAcc = dictionaryByOccurrences.get(current) match {
      case None => acc
      case Some(words) => acc.flatMap(sentence => words.map(word => sentence :+ word))
    }
    val newTail = tail.map(o => subtract(o, current)).filter(o => o.nonEmpty).filter(o => dictionaryByOccurrences.get(o).isDefined)
    newTail.flatMap(o => iterateCombination(newTail, newAcc, o))
}
*/

//iterateCombination(combinesLR, List(List()))
//Linux
val current001 = List(('r', 1), ('u', 1), ('l', 1), ('e', 1), ('z', 1)).sortBy(_._1)
val words = dictionaryByOccurrences.getOrElse(current001, List())

combinesLR.size

combinesLR.map(o => subtract(o, current001)).count(o => o.nonEmpty)

val res007 = combinesLR.map(occurences => subtract(occurences, current001))
  .map(occurences => subtract(occurences, List(('i', 1), ('l', 1), ('n', 1), ('u', 1), ('x', 1))))
  //.map(occurences => subtract(occurences, List(('z', 1), ('l', 1), ('u', 2))))
  .filter(occurences => occurences.nonEmpty)

def first(combinations: List[Occurrences], current: Occurrences, length: Int): List[Sentence] = {
  val words = dictionaryByOccurrences.getOrElse(current, List())
  combinations.map(o => subtract(o, current))
    .filter(o => o.nonEmpty)
    .map(o => dictionaryByOccurrences.getOrElse(o, List()))
    .filter(o => o.nonEmpty)
    .flatMap(sentence => words.map(word => word :: sentence))
    .filter(sentences => sentences.mkString.length == length)
}

val firstResult = first(combinesLR, current001, 10)
val firstResult01 = first(combinesLR, List(('r', 1), ('e', 1), ('x', 1)).sortBy(_._1), 10)




def iterateCombination(combinations: List[Occurrences], length: Int): List[Sentence] = {
  def iterate(index: Int, acc: List[Sentence]): List[Sentence] = {
    if (index < combinations.size) {
      val current = combinations(index)
      iterate(index + 1, acc ::: first(combinations, current, length))
    } else {
      acc
    }
  }

  iterate(0, List())
}

val resultOfResults = iterateCombination(combinations(sentenceOccurrences(List("Linux", "rulez"))), 10)


def testFunction(combinations: List[Occurrences], current: Occurrences): List[Sentence] = {
  val words = dictionaryByOccurrences.getOrElse(current, List())

  def iterate(combinations: List[Occurrences], current: Occurrences, acc: List[Sentence]): List[Sentence] = {
    val newAcc = acc.flatMap(sentence => words.map(word => word :: sentence))
    if (combinations.isEmpty) {
      newAcc
    } else {
      val newCombinations = combinations.map(o => subtract(o, current)).filter(o => o.nonEmpty)
      iterate(newCombinations, newCombinations.head, newAcc)
    }
  }

  iterate(combinations, current, List())
}

val rex = List(('r', 1), ('e', 1), ('x', 1)).sortBy(_._1)
val lin = List(('l', 1), ('i', 1), ('n', 1)).sortBy(_._1)
val zulu = List(('l', 1), ('u', 2), ('z', 1)).sortBy(_._1)
val wordsRex = dictionaryByOccurrences.getOrElse(rex, List())
//val resTest = testFunction(combinations(sentenceOccurrences(List("Linux", "rulez"))), rex)

combinations(sentenceOccurrences(List("Linux", "rulez")))
  .map(o => subtract(o, rex))
  .map(o => subtract(o, lin))
  .map(o => subtract(o, zulu)).filter(o => o.nonEmpty)
  .filter(o => o.nonEmpty)
/*.map(o => dictionaryByOccurrences.getOrElse(o, List()))
.filter(o => o.nonEmpty)
.flatMap(sentence => words.map(word => word :: sentence))
.filter(sentences => sentences.mkString.length == 10)*/
