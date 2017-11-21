import forcomp.Anagrams._

val first = List("a", "b", "c")
val second = List("e", "d")

first.zip(second)

val anas = List(
  List("Rex", "Lin", "Zulu"),
  List("nil", "Zulu", "Rex"),
  List("Rex", "nil", "Zulu"),
  List("Zulu", "Rex", "Lin"),
  List("null", "Uzi", "Rex"),
  List("Rex", "Zulu", "Lin"),
  List("Uzi", "null", "Rex"),
  List("Rex", "null", "Uzi"),
  List("null", "Rex", "Uzi"),
  List("Lin", "Rex", "Zulu"),
  List("nil", "Rex", "Zulu"),
  List("Rex", "Uzi", "null"),
  List("Rex", "Zulu", "nil"),
  List("Zulu", "Rex", "nil"),
  List("Zulu", "Lin", "Rex"),
  List("Lin", "Zulu", "Rex"),
  List("Uzi", "Rex", "null"),
  List("Zulu", "nil", "Rex"),
  List("rulez", "Linux"),
  List("Linux", "rulez")
)
anas.map(list => list.mkString.length)
List("Linux", "rulez").mkString.length


val combines = combinations(sentenceOccurrences(List("Linux", "rulez")))

for (
  combin <- combines;
  occurency <- combin
) yield dictionaryByOccurrences.getOrElse(List(occurency), List("::NULL::"))


dictionaryByOccurrences.get(List(('e', 1), ('r', 1), ('x', 1)))
dictionaryByOccurrences.get(List(('i', 1), ('l', 1), ('n', 1)))
dictionaryByOccurrences.get(List(('l', 1), ('u', 2), ('z', 1)))

combines.map(occurences => subtract(occurences, List(('e', 1), ('r', 1), ('x', 1))))
  .map(occurences => subtract(occurences, List(('i', 1), ('l', 1), ('n', 1))))
  .map(occurences => subtract(occurences, List(('l', 1), ('u', 2), ('z', 1))))
  .filter(occurences => occurences.nonEmpty)



combines.map(occurences => subtract(occurences, List(('e', 1), ('r', 1), ('x', 1))))
  .filter(occurences => occurences.nonEmpty)


def secondFunction(combines: List[Occurrences]): Option[Occurrences] = {
  combines.filter(occur => dictionaryByOccurrences.get(occur).isDefined) match {
    case Nil => None
    case head :: tail => Option(head)
  }
}

def firstFunction(current: Occurrences, combines: List[Occurrences], acc: List[Sentence]):
List[Sentence] =
  combines match {
    case Nil => acc
    case list =>
      dictionaryByOccurrences.get(current) match {
        case None => acc
        case Some(words) => /*acc.flatMap(sentence => words.map(word => sentence :+ word))*/
          val other = list
            .map(occurs => subtract(occurs, current))
            .filter(occurs => occurs.nonEmpty)
            .filter(occur => dictionaryByOccurrences.get(occur).isDefined)
          other.flatMap(occurs =>
            firstFunction(occurs, other, acc.flatMap(sentence => words.map(word => sentence :+ word))))
      }
  }

val combinesLR = combinations(sentenceOccurrences(List("Linux", "rulez")))
val firstResult = firstFunction(List(('e', 1), ('r', 1), ('x', 1)), combinesLR, List(List()))
//.filter(sentence => sentence.mkString.length == 10)


val words = List("assa", "foo")
List(List()).flatMap(sentence => words.map(word => sentence :+ word))

val secondCombines = combinesLR.map(occurences => subtract(occurences, List(('e', 1), ('r', 1), ('x', 1))))
  .filter(occurences => occurences.nonEmpty)
  .filter(occur => dictionaryByOccurrences.get(occur).isDefined)

secondCombines.map(occur => dictionaryByOccurrences.get(occur))

/*val third = secondCombines.map(occurs => subtract(occurs, List(('l', 1), ('u', 1), ('z', 1))))
  .filter(occurences => occurences.nonEmpty)
  .filter(occur => dictionaryByOccurrences.get(occur).isDefined)
val thirdWords = third.map(occur => dictionaryByOccurrences.get(occur))
*/


/*for (
  combination <- combinesLR;
  occur <- combination if ;
) yield dictionaryByOccurrences.get(occur)*/


for (i <- 0 until 10;
     j <- i until 10 if i + j == 3)
  print(s"($i, $j)")

