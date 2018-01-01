trait Term
case class Con(a: Int) extends Term
case class Div(t: Term, u: Term) extends Term


val ans: Term =
  Div(
    Div(
      Con(1932),
      Con(2)
    ),
    Con(23)
  )


val err: Term =
  Div(
    Con(1),
    Con(0)
  )


def eval(t: Term): Int =
  t match {
    case Con(a) => a
    case Div(t, u) => eval(t) / eval(u)
  }


eval(ans) // a: Int = 42
eval(err) // # java.lang.ArithmeticException: / by zero...

















































// Exceptions?
type Exception = String
trait M[A]
case class Raise[A](e: Exception) extends M[A]
case class Return[A](e: A) extends M[A]


def eval[A](s: Term): M[Int] =
  s match {
    case Con(a) => Return(a)
    case Div(t, u) =>
      eval(t) match {
        case Raise(e) => Raise(e)
        case Return(a) =>
          eval(u) match {
            case Raise(e) => Raise(e)
            case Return(b) =>
              if (b == 0) Raise("Divide by Zero")
              else Return (a / b)
          }
      }
  }


eval(ans) // # Return(42) : M[Int]
eval(err) // # Raise("Divide by Zero") : M[Int]

















































// State
type State = Int
type M[A] = State => (A, State)


def eval(s: Term): M[Int] =
  s match {
    case Con(a) => x => (a, x)
    case Div(t, u) =>
      x =>
        val (a, y) = eval(t)(x)
        val (b, z) = eval(u)(y)
        (a / b, z + 1)
  }


eval(ans)(0) // # (42, 2) : (Int, State)

















































// Writer
type Output = String
type M[A] = (Output, A)


def eval(s: Term): M[Int] =
  s match {
    case Con(a) => (toOutput(s, a), a)
    case Div(t, u) =>
      val (x, a) = eval(t)
      val (y, b) = eval(u)
      (x + y + toOutput(s, a / b), a / b)
  }


def toOutput(t: Term, a: Int): Output =
  t + "=" + a + "\n"


eval(ans)
/* # ("Con(1932)=1932
       Con(2)=2
       Div(Con(1932),Con(2))=966
       Con(23)=23
       Div(Div(Con(1932),Con(2)),Con(23))=42
      ",42) : (Output, Int)
*/
