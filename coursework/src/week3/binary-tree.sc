import example._

val t1 = new NonEmpty(2, new Empty, new Empty)
val t2 = new NonEmpty(3, new Empty, new Empty)
val t3 = t1 incl 4

t2 union t3

def f(xs: List[NonEmpty], x: Empty) = xs prepend x

