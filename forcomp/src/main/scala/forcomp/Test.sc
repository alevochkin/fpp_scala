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

List("Rex", "Lin", "Zulu").mkString