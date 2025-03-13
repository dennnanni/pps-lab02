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
    val plus = "+"
    val times = "*"
    val openPar = "("
    val closePar = ")"

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
      assertEquals(openPar + n1.toString + plus + n2.toString + closePar,
        show(e))
    }

    @Test def testMultiplyToString(): Unit = {
      val e = Multiply(Literal(n1), Literal(n2))
      assertEquals(openPar + n1.toString + times + n2.toString + closePar,
        show(e))
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

    @Test def testAddToStringWithNegativeValues(): Unit = {
      val e = Add(Literal(negative1), Literal(negative2))
      assertEquals(openPar * 2 + negative1 + closePar +
        plus + openPar + negative2 + closePar * 2, show(e))
    }

  enum Expr:
    case Literal(n: Int)
    case Add(e1: Expr, e2: Expr)
    case Multiply(e1: Expr, e2: Expr)

  object Expr:
    val plus = "+"
    val times = "*"
    val openPar = "("
    val closePar = ")"

    def evaluate(e: Expr): Int = e match
      case Literal(n) => n
      case Add(e1, e2) => evaluate(e1) + evaluate(e2)
      case Multiply(e1, e2) => evaluate(e1) * evaluate(e2)

    def show(e: Expr): String = e match
      case Literal(n) if n >= 0 => n.toString
      case Literal(n) if n < 0 => openPar + n.toString + closePar
      case Add(e1, e2) => openPar + show(e1) + plus + show(e2) + closePar
      case Multiply(e1, e2) => openPar + show(e1) + times + show(e2) + closePar

