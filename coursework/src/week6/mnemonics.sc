import scala.io.Source

val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

val words = in.getLines.toList filter (word => word forall (char => char.isLetter))

val mnemonics = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

val charCode = (for (key <- mnemonics.keys)
  yield mnemonics(key) map(l => (l -> key))).flatten.toMap

def wordCode(word: String) = word.toUpperCase map charCode

wordCode("Java")

def wordsForNum: Map[String,Seq[String]] =
  words groupBy wordCode withDefaultValue Seq()

wordsForNum("5282")

def encode(number: String): Set[List[String]] = {
  if(number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest
  }.toSet
}

encode("7225247386")

def translate(number: String): Set[String] = {
  encode(number) map (_ mkString " ")
}

translate("7225247386")

