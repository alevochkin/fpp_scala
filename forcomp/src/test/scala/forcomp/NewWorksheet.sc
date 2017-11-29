import forcomp.Anagrams.{combinations, _}

def testFunction(combinations: List[Occurrences], current: Occurrences): List[Sentence] = {

  def nextWords(combinations: List[Occurrences]): List[Word] = combinations match {
    case Nil => List()
    case head :: tail => dictionaryByOccurrences.get(head) match {
      case None => nextWords(tail)
      case Some(words) => words
    }
  }



  def iterate(combinations: List[Occurrences], current: Occurrences, acc: List[Sentence]): List[Sentence] = {
    /*val newAcc = acc.flatMap(sentence => words.map(word => word :: sentence))*/
    val words = dictionaryByOccurrences.getOrElse(current, List())
    /*println("::WORDS::")
    println(words)*/
    /*println("::ACC::")
    println(acc)*/
    val newAcc = acc.flatMap(sentence => words.map(word => word :: sentence))
    /*println("::NEW_ACC::")
    println(newAcc)*/

    val newCombinations = combinations.map(o => subtract(o, current)).filter(o => o.nonEmpty)
    if (newCombinations.isEmpty) {
      newAcc
    } else {
      iterate(newCombinations, newCombinations.head, newAcc)
    }
  }

  iterate(combinations, current, List(Nil))
}

val rex = List(('r', 1), ('e', 1), ('x', 1)).sortBy(_._1)
val lin = List(('l', 1), ('i', 1), ('n', 1)).sortBy(_._1)
val zulu = List(('l', 1), ('u', 2), ('z', 1)).sortBy(_._1)
val wordsRex = dictionaryByOccurrences.getOrElse(rex, List())
val resTest = testFunction(combinations(sentenceOccurrences(List("Linux", "rulez"))), rex)

//List()

def merge(acc: List[Sentence], words: List[Word]): List[Sentence] = {
  acc.flatMap(sentence => words.map(word => word :: sentence))
}

def foo(combinations: List[Occurrences], acc: List[Sentence]): List[Sentence] = combinations match {
  case Nil => acc
  case head :: tail => dictionaryByOccurrences.get(head) match {
    case None => foo(tail, acc)
    case Some(words) => foo(combinations.map(o => subtract(o, head)), merge(acc, words))
  }
}

def first(combinations: List[Occurrences], acc: List[Sentence], index: Int): List[Sentence] = {
    if(index < combinations.size) {
      val current = combinations(index)
      //println("Current: " + current)
      dictionaryByOccurrences.get(current) match {
        case None => first(combinations, acc, index + 1)
        case Some(words) =>
          val newFoo = foo(combinations.map(o => subtract(o, current)), List(words))
          //println("Words: " + words)
          //println("Foo: " + newFoo)
          first(combinations, acc ::: newFoo, index + 1)
      }
    } else {
      acc
    }
}


val linuxRulez = combinations(sentenceOccurrences(List("Linux", "rulez")))
val linuxRulexRes = first(linuxRulez, List(), 0)


linuxRulez.map(o => subtract(o, rex))
val fooRes = foo(linuxRulez.map(o => subtract(o, rex)), List(dictionaryByOccurrences.getOrElse(rex, List())))



/*for(
  combine <- linuxRulez;
  element <- linuxRulez.map(c => subtract(c, combine))
) yield dictionaryByOccurrences.getOrElse(element, List("::NONE::"))*/
//for(combine <- linuxRulez; others = linuxRulez.map(c => subtract(c, combine)); other <- others)

/*def combinations(occurrences: List[Occurrences]): List[Sentence] = occurrences match {
  case List() => List(Nil)
  case (c, n) :: others =>
    val tails = combinations(others)
    tails ::: (for {
      j <- tails
      i <- 1 to n
    } yield (c, i) :: j)
}*/

val first = for(combination <- linuxRulez if dictionaryByOccurrences.get(combination).isDefined) yield combination

first.flatMap(combination => dictionaryByOccurrences.get(combination))

for {
  comb1 <- first
  comb2 <- first
  if comb1 != comb2
} yield (dictionaryByOccurrences.get(comb1), dictionaryByOccurrences.get(comb2))
