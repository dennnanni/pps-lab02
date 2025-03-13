package u02

import org.junit.*
import org.junit.Assert.*
import u02.Lab2.Expr
import u02.Lab2.Expr.*

object Lab2 extends App:

  // Task 4
  class Task4Test:
    @Test def testLiteralEvaluation(): Unit = {
      val e = Expr.Literal(5)
      assertEquals(5, evaluate(e))
    }

    @Test def testAddEvaluationWithLiterals(): Unit = {
      val e = Expr.Add(Expr.Literal(3), Expr.Literal(2))
      assertEquals(5, evaluate(e))
    }

    @Test def testMultiplyEvaluationWithLiterals(): Unit = {
      val e = Expr.Multiply(Expr.Literal(3), Expr.Literal(2))
      assertEquals(6, evaluate(e))
    }

    @Test def testAddWithSubExpr(): Unit = {
      val e = Expr.Add(Expr.Add(Expr.Literal(3), Expr.Literal(2)),
        Expr.Multiply(Expr.Literal(3), Expr.Literal(2)))
      assertEquals(11, evaluate(e))
    }

  enum Expr:
    case Literal(n: Int)
    case Add(e1: Expr, e2: Expr)
    case Multiply(e1: Expr, e2: Expr)

  object Expr:
    def evaluate(e: Expr): Int = e match
      case Expr.Literal(n) => n
      case Expr.Add(e1, e2) => evaluate(e1) + evaluate(e2)
      case Expr.Multiply(e1, e2) => evaluate(e1) * evaluate(e2)