package u02

import org.junit.*
import org.junit.Assert.*
import u02.Lab2.Expr
import u02.Lab2.Expr.*

object Lab2 extends App:

  // Task 4
  class Task4Test:
    val n1 = 3
    val n2 = 2
    val negative1 = -3
    val negative2 = -2

    @Test def testLiteralEvaluation(): Unit = {
      val e = Literal(n1)
      assertEquals(n1, evaluate(e))
    }

    @Test def testAddEvaluationWithLiterals(): Unit = {
      val e = Add(Literal(n1), Literal(n2))
      assertEquals(n1 + n2, evaluate(e))
    }

    @Test def testMultiplyEvaluationWithLiterals(): Unit = {
      val e = Multiply(Literal(n1), Literal(n2))
      assertEquals(n1 * n2, evaluate(e))
    }

    @Test def testAddWithSubExpr(): Unit = {
      val e = Add(Add(Literal(n1), Literal(n2)),
        Multiply(Literal(n1), Literal(n2)))
      assertEquals((n1 + n2) + n1 * n2, evaluate(e))
    }

    @Test def testMultiplyWithSubExpr(): Unit = {
      val e = Multiply(Add(Literal(n1), Literal(n2)),
        Add(Literal(n1), Literal(n2)))
      assertEquals((n1 + n2) * (n1 + n2), evaluate(e))
    }

    @Test def testLiteralToString(): Unit = {
      val e = Literal(n1)
      assertEquals(n1.toString, show(e))
    }

    @Test def testAddToString(): Unit = {
      val e = Add(Literal(n1), Literal(n2))
      assertEquals("(" + n1.toString + "+" + n2.toString + ")", show(e))
    }

    @Test def testMultiplyToString(): Unit = {
      val e = Multiply(Literal(n1), Literal(n2))
      assertEquals("(" + n1.toString + "*" + n2.toString + ")", show(e))
    }

    @Test def testLiteralWithNegativeValue(): Unit = {
      val e = Literal(negative1)
      assertEquals(negative1, evaluate(e))
    }

    @Test def testAddWithNegativeValues(): Unit = {
      val e = Add(Literal(negative1), Literal(negative2))
      assertEquals(negative1 + negative2, evaluate(e))
    }

    @Test def testMultiplyWithNegativeValues(): Unit = {
      val e = Multiply(Literal(negative1), Literal(negative2))
      assertEquals(negative1 * negative2, evaluate(e))
    }

  enum Expr:
    case Literal(n: Int)
    case Add(e1: Expr, e2: Expr)
    case Multiply(e1: Expr, e2: Expr)

  object Expr:
    def evaluate(e: Expr): Int = e match
      case Literal(n) => n
      case Add(e1, e2) => evaluate(e1) + evaluate(e2)
      case Multiply(e1, e2) => evaluate(e1) * evaluate(e2)

    def show(e: Expr): String = e match
      case Literal(n) => n.toString
      case Add(e1, e2) => "(" + show(e1) + "+" + show(e2) + ")"
      case Multiply(e1, e2) => "(" + show(e1) + "*" + show(e2) + ")"

