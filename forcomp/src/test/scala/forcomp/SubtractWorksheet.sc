val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val r = List(('r', 1))
val lad = List(('a', 1), ('d', 1), ('l', 1))

lard ::: r.map { case (key, value) => (key, -value) }

/*.foldLeft(Map.empty[Char, Int])({
  case (acc, (key, value)) => acc + (key -> (acc.getOrElse(key, 0) + value))
})*/


lard ::: r.map { case (key, value) => (key, -value) }


List(('a', 1), ('d', 1), ('l', 1), ('r', 1), ('r', -1)).foldLeft(Map.empty[Char, Int])
{ case (acc, (key, value)) => acc + ((key, acc.getOrElse(key, 0) + value)) }
  .toList.filter{case(key, value) => value > 0}


(lard ::: r.map { case (key, value) => (key, -value) }).foldLeft(Map.empty[Char, Int])
{case (acc, (key, value)) => acc + (key -> (acc.getOrElse(key, 0) + value))}

  /*.foldLeft(Map.empty[Char, Int])({
    case (acc, (key, value)) => acc + (key -> (acc.getOrElse(key, 0) + value))
  })*/
/*.toList.filter(tuple => tuple._2 > 0)*/