class Poly(val terms: Map[Int,Double]) {
  def this(bindings: (Int,Double)*) = this(bindings.toMap)

  val termsWithDefault = this.terms withDefaultValue 0.0
  def sum (other: Poly): Poly = new Poly({
    (for (keys <- this.terms.keys ++ other.terms.keys)
    yield {
      (this.terms.get(keys), other.terms.get(keys)) match {
        case (Some(x), Some(y)) => (keys, (x + y))
        case (Some(x), None) => (keys,x)
        case (None, Some(y)) => (keys,y)
      }
    }).toMap
  })

  def otherSum (other: Poly): Poly = new Poly(termsWithDefault ++ (other.terms map adjust))
    def adjust(terms: (Int,Double)) = {
      val (key,value) = terms
      key -> (value + termsWithDefault(key))
    }

  def + (other: Poly): Poly = {
    new Poly((other.terms foldLeft termsWithDefault)(addTerm))
  }

  def addTerm(terms: Map[Int,Double], term: (Int,Double)) = {
    terms.updated(term._1,terms(term._1) + term._2)
  }

  override def toString =
    (for((exp,coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
}
val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
(p1 + p2).toString == p1.otherSum(p2).toString
(p1 + p2).toString == p1.sum(p2).toString
p1.otherSum(p2).toString == p1.sum(p2).toString

