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



combines.map(occurences => subtract(occurences, List(('e', 1), ('r', 1), ('x', 1)))).filter(occurences => occurences.nonEmpty)


def secondFunction(combines: List[Occurrences]): Option[Occurrences] = {
  combines.filter(occur => dictionaryByOccurrences.get(occur).isDefined) match {
    case Nil => None
    case head::tail => Option(head)
  }
}

def firstFunction(current: Occurrences, combines: List[Occurrences], acc: List[Sentence]): List[Sentence] =
  combines match {
    case Nil => acc
    case list =>
      val other = list.map(occurences => subtract(occurences, current)).filter(occurences => occurences.nonEmpty)
      val words = dictionaryByOccurrences.getOrElse(current, List())
      secondFunction(other) match {
        case None => acc
        case Some(newCurrent) => firstFunction (newCurrent, other,
          if (acc.isEmpty) words.map (word => List (word) ) else acc)
      }
  }

val combinesLR = combinations(sentenceOccurrences(List("Linux", "rulez")))

firstFunction(List(('e', 1), ('r', 1), ('x', 1)), combinesLR, List())

combinesLR.map(occurences => subtract(occurences, List(('e', 1), ('r', 1), ('x', 1))))
  .filter(occurences => occurences.nonEmpty)
  .filter(occur => dictionaryByOccurrences.get(occur).isDefined)
.map(occur => dictionaryByOccurrences.get(occur))






