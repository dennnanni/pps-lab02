package u02

import org.junit.*
import org.junit.Assert.*
import task5.Optionals.Optional
import task5.Optionals.Optional.*
import u02.Lab2.Expr
import u02.Lab2.Expr.*

import scala.annotation.tailrec

object Lab2 extends App:

  //Task 1 svolto da sola
  def curriedMult(x: Double)(y: Double): Double = x * y
  // curriedMult(2, 3)
  // Usando il currying nella definizione non si può più richiamare la funzione usando la virgola.

  def sumAndMult(a: Double, b: Double)(c: Double): Boolean =
    (a + c) * b match
      case n if n > 15 => true
      case _ => false
  // Non importa l'ordine in cui si usano i parametri di un metodo curried.

  val stringa = "hello " + "world"
  // La concatenazione può essere fatta anche tramite +

  val i: (Int, Int => Int, Int => Int) => Int =
    (n, f1, f2) => f1(f2(n))
  val double: Int => Int = 2 * _
  i(6, double, _ + 1)
  // Posso passare funzioni già definite come parametro in cui viene richiesta una funzione.


  // Task 2
  // Positive come function literal
  val positive: Int => String = v => v match
    case n if n >= 0 => "positive"
    case _ => "negative"

  // Positive come function definition
  def positive(v: Int): String = v match
    case n if n >= 0 => "positive"
    case _ => "negative"

  // Neg con metodo
  def neg(f: String => Boolean): String => Boolean = s => !f(s)

  // Neg con function literal
  val negLiteral: (String => Boolean) => (String => Boolean) = f => s => !f(s)

  // Neg generica
  def genericNeg[A](f: A => Boolean): A => Boolean = v => !f(v)

  genericNeg(x => x == 3)(4)

  // Task 2 svolto da sola
  // Currying
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  p1(3)(3)(4) // true
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
  p2(3, 3, 5) // false

  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z

  p3(3)(3)(4)

  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  p4(3, 3, 4)

  // Compose con interi
  def compose(f: Int => Int, g: Int => Int): Int => Int = v => f(g(v))

  // Compose generico
  def composeGeneric[A, B, C](f: B => C, g: A => B): A => C = v => f(g(v))

  // Compose di tre funzioni
  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = x => f(g(h(x)))

  // Compose a tre riutilizzando compose a due
  def composeThreeReuse[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
    x => composeGeneric(f, composeGeneric(g, h))(x)

  
  // Task 3 svolto da sola
  // Funzione ricorsiva per la potenza
  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1
    case n if n > 0 => base * power(base, exponent - 1)

  // Potenza utilizzando tail recursion
  @tailrec
  def power(base: Double, exponent: Int, acc: Double): Double = exponent match
    case 0  => acc
    case n if n > 0  => power(base, exponent - 1, base * acc)

  // Inversione di un numero
  // Helper function
  @tailrec
  def buildNumber(n: Int, reversed: Int): Int = n match
    case n if n == 0 => reversed
    case _ => buildNumber(n / 10, reversed * 10 + n % 10)

  def reverseNumber(n: Int): Int = buildNumber(n, 0)


  // Task 4 svolto da sola
  class Task4Test:
    val n1 = 3
    val n2 = 2
    val negative1 = -3
    val negative2 = -2
    val plus = "+"
    val times = "*"
    val openPar = "("
    val closePar = ")"

    @Test def testLiteralEvaluation(): Unit =
      val e = Literal(n1)
      assertEquals(n1, evaluate(e))

    @Test def testAddEvaluationWithLiterals(): Unit =
      val e = Add(Literal(n1), Literal(n2))
      assertEquals(n1 + n2, evaluate(e))

    @Test def testMultiplyEvaluationWithLiterals(): Unit =
      val e = Multiply(Literal(n1), Literal(n2))
      assertEquals(n1 * n2, evaluate(e))

    @Test def testAddWithSubExpr(): Unit =
      val e = Add(Add(Literal(n1), Literal(n2)),
        Multiply(Literal(n1), Literal(n2)))
      assertEquals((n1 + n2) + n1 * n2, evaluate(e))

    @Test def testMultiplyWithSubExpr(): Unit =
      val e = Multiply(Add(Literal(n1), Literal(n2)),
        Add(Literal(n1), Literal(n2)))
      assertEquals((n1 + n2) * (n1 + n2), evaluate(e))

    @Test def testLiteralToString(): Unit =
      val e = Literal(n1)
      assertEquals(n1.toString, show(e))

    @Test def testAddToString(): Unit =
      val e = Add(Literal(n1), Literal(n2))
      assertEquals(openPar + n1.toString + plus + n2.toString + closePar,
        show(e))

    @Test def testMultiplyToString(): Unit =
      val e = Multiply(Literal(n1), Literal(n2))
      assertEquals(openPar + n1.toString + times + n2.toString + closePar,
        show(e))

    @Test def testLiteralWithNegativeValue(): Unit =
      val e = Literal(negative1)
      assertEquals(negative1, evaluate(e))

    @Test def testAddWithNegativeValues(): Unit =
      val e = Add(Literal(negative1), Literal(negative2))
      assertEquals(negative1 + negative2, evaluate(e))

    @Test def testMultiplyWithNegativeValues(): Unit =
      val e = Multiply(Literal(negative1), Literal(negative2))
      assertEquals(negative1 * negative2, evaluate(e))

    @Test def testAddToStringWithNegativeValues(): Unit =
      val e = Add(Literal(negative1), Literal(negative2))
      assertEquals(openPar * 2 + negative1 + closePar +
        plus + openPar + negative2 + closePar * 2, show(e))


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


  // Task 5 (ho riportato i test e le implementazioni che ho fatto io) svolto da sola
  class OptionalTest:
    val initValue = 0
    @Test def mapShouldReturnEmptyWhenEmpty(): Unit =
      val empty: Optional[Int] = Optional.Empty()
      val result = Optional.map(empty, _ + 1)
      assertTrue(Optional.isEmpty(result))

    @Test def mapShouldReturnTransformedValueWhenNonEmpty(): Unit =
      val nonEmpty = Optional.Maybe(initValue)
      val result = Optional.map(nonEmpty, _ + 1)
      assertEquals(1, Optional.orElse(result, -1))

    @Test def filterShouldReturnEmptyWhenEmpty(): Unit =
      val empty: Optional[Int] = Optional.Empty()
      val result = Optional.filter(empty)(_ > 2)
      assertTrue(Optional.isEmpty(result))

    @Test def filterShouldReturnValueWhenPredicateIsTrue(): Unit =
      val nonEmpty = Optional.Maybe(initValue)
      val result = Optional.filter(nonEmpty)(_ == initValue)
      assertEquals(0, Optional.orElse(result, -1))

    @Test def filterShouldReturnEmptyWhenPredicateIsFalse(): Unit =
      val nonEmpty = Optional.Maybe(initValue)
      val result = Optional.filter(nonEmpty)(_ > initValue)
      assertTrue(Optional.isEmpty(result))


  def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
    case Empty() => Empty()
    case Maybe(value) => Maybe(f(value))

  def filter[A](optional: Optional[A])(f: A => Boolean): Optional[A] = optional match
    case Maybe(value) if f(value) => Maybe(value)
    case _ => Empty()

