trait Expr

case class Number(n: Int) extends Expr

case class Var(x: String) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

val one = Number(1)


def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1,e2) => eval(e1) + eval(e2)
  case Prod(e1,e2) => eval(e1) * eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n) => s"$n"
  case Var(x) => x
  case Sum(e1,e2) => show(e1) + " + " + show(e2)
  case Prod(Sum(e1,e2),e3) => "(" + show(e1) + " + " + show(e2) + ") * " + show(e3)
  case Prod(e1,Sum(e2,e3)) => show(e1) + " * (" + show(e2) + " + " + show(e3) + ")"
  case Prod(e1,e2) => show(e1) + " * " + show(e2)
}

show(Prod(Sum(Number(11),Var("y")),Var("x")))
show(Prod(Var("x"),Sum(Number(11),Var("y"))))
show(Sum(Var("x"),Prod(Number(11),Var("y"))))
show(one)

