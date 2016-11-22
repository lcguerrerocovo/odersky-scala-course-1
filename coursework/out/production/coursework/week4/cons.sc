import example._

def nth[T](list : List[T], n : Int) : T = {
  if(list.isEmpty) throw new IndexOutOfBoundsException
  else if(n == 0) list.head
  else nth(list.tail, n - 1)
}

val list = List(1,2)

nth(list,0)

new Cons(1,new Cons(2,new Cons(3,new Nil)))

list.prepend(4)

