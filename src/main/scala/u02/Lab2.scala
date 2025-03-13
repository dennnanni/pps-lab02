package u02

import org.junit.*
import org.junit.Assert.*
import u02.Lab2.Expr.*

object Lab2 extends App:

  // Task 4
  class Task4Test:
    @Test def testLiteralEvaluation(): Unit = {
      val e = Expr.Literal(5)
      assertEquals(5, evaluate(e))
    }

  enum Expr:
    case Literal(n: Int)
    case Add(n1: Expr, n2: Expr)
    case Multiply(n1: Expr, n2: Expr)

  object Expr:
    def evaluate(e: Expr): Int = e match
      case Expr.Literal(n) => n