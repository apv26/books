package essential_scala

import java.util.Date

import scala.annotation.tailrec

object Ex4Traits extends App {

  //  4.1.4.1 Cats,and MoreCats
  trait Feline {
    val colour: String
    val sound: String
  }

  case class Tiger(colour: String, maneSize: Int) extends Feline {
    val sound = "roar"
  }

  case class Lion(colour: String) extends Feline {
    val sound: String = "roar"
  }

  case class Panther(colour: String) extends Feline {
    val sound: String = "roar"
  }

  case class Cat(colour: String, food: String) extends Feline {
    val sound: String = "meow"
  }

  // 4.1.4.2 Shaping Up With Traits
  trait Shape {
    def sides: Int

    def perimeter: Double

    def area: Double
  }

  case class Circle1(
    radius: Double
  ) extends Shape {
    val sides = 1
    val perimeter = math.Pi * 2 * radius
    val area = math.Pi * radius * radius
  }

  case class Rectangle1(a: Double, b: Double) extends Shape {
    val sides = 4
    val perimeter = 2 * (a + b)
    val area = a * b
  }

  case class Square(
    a: Double
  ) extends Shape {
    val sides = 4
    val perimeter = 4 * a
    val area = a * a
  }

  //  4.5.6.3 Email
  sealed trait Visitor {
    def id: String
    def createdAt: Date
    def age: Long = new Date().getTime - createdAt.getTime
  }
  final case class Anonymous(
    id: String,
    createdAt: Date = new Date()
  ) extends Visitor
  final case class User(
    id: String,
    email: String,
    createdAt: Date = new Date()
  ) extends Visitor
  // not only depends on other fields and methods in a class
  //  we generally prefer sealed traits
  object EmailService {
    def email(visitor: Visitor): Unit = {
      visitor match {
        case a: Anonymous => ()
        case u: User      => println("send " + u.email)
      }
    }
  }

  // 4.6.3.2 The Forest of Trees
  // pattern matching
  sealed trait Tree
  final case class Node(left: Tree, right: Tree) extends Tree
  final case class Leaf(element: Int) extends Tree

  object Tree {
    def mkSpace(i: Int) = Array.fill(i)(" ").mkString("")
    def prettyPrint(t: Tree): String = {
      @tailrec
      def inner(l: List[(Tree, Int)], indent: Int, acc: String): String =
        l match {
          case Nil => acc
          case (Leaf(v), ind) :: ls =>
            inner(ls, ind, acc + mkSpace(ind) + v + "\n")
          case (Node(a, b), ind) :: ls =>
            inner(
              (a, ind + 1) :: (b, ind + 1) :: ls,
              ind,
              acc + mkSpace(ind) + "\n"
            )
        }
      inner(List((t, 0)), 0, "")
    }
    def sum(t: Tree): Int = {
      @tailrec
      def inner(l: List[Tree], acc: Int): Int =
        l match {
          case Nil              => acc
          case Leaf(v) :: ls    => inner(ls, acc + v)
          case Node(a, b) :: ls => inner(a :: b :: ls, acc)
        }
      inner(List(t), 0)
    }
    def map(t: Tree)(f: Int => Int): Tree = {

      case object BranchStub extends Tree

      @tailrec
      def mapImp(toVisit: List[Tree], acc: Vector[Tree]): Vector[Tree] =
        if (toVisit.isEmpty) acc
        else {
          toVisit.head match {
            case Leaf(v) =>
              val leafRes = Leaf(f(v))
              mapImp(
                toVisit.tail,
                acc :+ leafRes
              )
            case Node(l, r) =>
              mapImp(l :: r :: BranchStub :: toVisit.tail, acc)
            case BranchStub =>
              mapImp(
                toVisit.tail,
                acc.dropRight(2) ++ Vector(acc.takeRight(2).reduce(Node))
              )
          }
        }

      mapImp(t :: Nil, Vector.empty).head

    }
    def double(t: Tree): Tree = map(t)(_ * 2)
  }
  val t =
    Node(
      Node(
        Node(
          Leaf(5),
          Leaf(4)
        ),
        Leaf(3)
      ),
      Leaf(2)
    )
  println(Tree.prettyPrint(t))
  println(Tree.sum(t))
  println(Tree.prettyPrint(Tree.double(t)))

  // using polymorphism
  sealed trait Tree1 {
    def prettyPrint(indent: String): Unit
    def sum(): Int
    def double(): Tree1
  }
  case class Node1(left: Tree1, right: Tree1) extends Tree1 {
    def prettyPrint(indent: String): Unit = {
      left.prettyPrint(indent + " ")
      right.prettyPrint(indent + " ")
      println(indent + ".")
    }
    def sum(): Int = left.sum() + right.sum()
    def double(): Tree1 = Node1(left.double(), right.double())
  }
  case class Leaf1(element: Int) extends Tree1 {
    def prettyPrint(indent: String): Unit = println(indent + element)
    def sum(): Int = element
    def double(): Tree1 = Node1(Leaf1(element), Leaf1(element))
  }
  val t1 =
    Node1(
      Node1(
        Node1(
          Leaf1(5),
          Leaf1(4)
        ),
        Leaf1(3)
      ),
      Leaf1(2)
    )
  t1.prettyPrint("")
  println(t1.sum())
  t1.double().prettyPrint("")

  // 4.2.2.2 The Color and the Shape
  val darkBlue = Colour(0, 0, 102)
  val lightYellow = Colour(255, 255, 204)
  println(s"darkBlue id dark: ${darkBlue.isDark}")
  println("lightYellow id dark: " + lightYellow.isDark)
  println(Draw(Circle(10, Yellow)))
  println(Draw(Rectangle(2, 4, Colour(23, 43, 55))))

  // 4.2.2.3 A Short Division Exercise
  val x = Divide(1, 2)
  val y = Divide(1, 0)
  println(x)
  println(y)
  println(Divide.perform(1, 2))
  println(Divide.perform(1, 0))

  // 4.5.6.2 Calculation
  assert(Calculator.+(Success(1), 1) == Success(2))
  assert(Calculator.-(Success(1), 1) == Success(0))
  assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))
  assert(Calculator./(Success(4), 2) == Success(2))
  assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
  assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))

  // 4.6.3.1 A List of Methods
  val example = Pair(1, Pair(2, Pair(3, End)))
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End.length == 0)

  assert(example.product == 6)
  assert(example.tail.product == 6)
  assert(End.product == 1)

  assert(example.double == Pair(2, Pair(4, Pair(6, End))))
  assert(example.tail.double == Pair(4, Pair(6, End)))
  assert(End.double == End)

  assert(
    Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval == Error(
      "Square root of negative number"
    )
  )
  assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Value(4.0))
  assert(Division(Number(4), Number(0)).eval == Error("Division by zero"))
}

// 4.1.4.3 Shaping Up 2 (Da Streets)
sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
  def color: Color
}

sealed trait Rectangular extends Shape {
  def a: Double
  def b: Double
  val sides = 4
  override val perimeter = 2 * (a + b)
  override val area = a * b
}

case class Rectangle(a: Double, b: Double, color: Color) extends Rectangular

case class Square(
  a: Double,
  color: Color
) extends Rectangular {
  val b = a
}

case class Circle(
  radius: Double,
  color: Color
) extends Shape {
  val sides = 1
  val perimeter = math.Pi * 2 * radius
  val area = math.Pi * radius * radius
}

// 4.2.2.2 The Color and the Shape
// 4.2.2.1 Printing Shapes
object Draw {
  def apply(s: Shape): String = s match {
    case Circle(r, c) => s"A ${Draw(c)} circle of radius ${r}cm"
    case Rectangle(a, b, c) =>
      s"A ${Draw(c)} rectangle of width ${a}cm and height ${b}cm"
  }
  def apply(c: Color): String = c match {
    case Red    => "red"
    case Yellow => "yellow"
    case Pink   => "pink"
    case c      => if (c.isDark) "dark" else "light"
  }
}
//[warn] C:\work\books\src\main\scala\essential_scala\Ex4Traits.scala:232:25: match may not be exhaustive.
//[warn] It would fail on the following input: Square(_)
//[warn]   def apply(s: Shape) = s match {

// 4.2.2.2 The Color and the Shape
sealed trait Color {
  def r: Int
  def g: Int
  def b: Int
  def isDark: Boolean =
    (r * 299 + g * 587 + b * 114) / 1000 < 125
}
final case class Colour(r: Int, g: Int, b: Int) extends Color

case object Red extends Color {
  val r = 255
  val g = 0
  val b = 0
}
case object Yellow extends Color {
  val r = 255
  val g = 0
  val b = 0
}
case object Pink extends Color {
  val r = 248
  val g = 24
  val b = 148
}

// 4.2.2.3 A Short Division Exercise
sealed trait DivisionResult
final case class Finite(res: Int) extends DivisionResult
case object Infinite extends DivisionResult
object Divide {
  def apply(n1: Int, n2: Int) =
    if (n2 == 0) Infinite else Finite(n1 / n2)
  def perform(n1: Int, n2: Int) =
    apply(n1, n2) match {
      case Infinite  => "Division by zero is denied"
      case Finite(x) => s"Result: $x"
    }
}

// 4.5.6.1 Traffic Lights
// 4.4.4.1 Stop on a Dime
// is a or, sum type pattern, ADT
sealed trait TrafficLight {
  def next1: TrafficLight
  def next: TrafficLight = this match {
    case Red1    => Green1
    case Green1  => Yellow1
    case Yellow1 => Red1
  }
}
case object Red1 extends TrafficLight {
  def next1: TrafficLight = Green1
}
case object Green1 extends TrafficLight {
  def next1: TrafficLight = Yellow1
}
case object Yellow1 extends TrafficLight {
  def next1: TrafficLight = Red1
}
// > Do you think it is better to implement this method next inside or outside the class?
// Inside, because all we need contains in class and we don't want multiple implementation of it.
// > If inside, would you use pattern matching or polymorphism? Why?
// I would use pattern matching, because I already know all subtypes and I can easy add new method.

// 4.5.6.2 Calculation
// 4.4.4.2 Calculator
// is a or, sum type pattern, ADT
sealed trait Calculation
final case class Success(result: Int) extends Calculation
final case class Failure(message: String) extends Calculation

object Calculator {
  def +(c: Calculation, i: Int): Calculation = c match {
    case Success(j) => Success(j + i)
    case Failure(f) => Failure(f)
  }
  def -(c: Calculation, i: Int): Calculation = c match {
    case Success(j) => Success(j - i)
    case Failure(f) => Failure(f)
  }
  def /(c: Calculation, i: Int): Calculation = c match {
    case Success(j) if i == 0 => Failure("Division by zero")
    case Success(j)           => Success(j / i)
    case Failure(f)           => Failure(f)
  }
}

// 4.4.4.3 Water, Water, Everywhere
sealed trait Source
case object Well extends Source
case object Spring extends Source
case object Tap extends Source
// has a and, product type pattern, ADT
final case class BottledWater(size: Int, source: Source, carbonated: Boolean)

// 4.6.3.1 A List of Methods
sealed trait IntList {

  def length: Int = {
    @tailrec
    def iter(acc: Int, l: IntList): Int = {
      l match {
        case End           => acc
        case Pair(_, tail) => iter(acc + 1, tail)
      }
    }
    iter(0, this)
  }
  def product: Int = {
    @tailrec
    def iter(acc: Int, l: IntList): Int = {
      l match {
        case End              => acc
        case Pair(head, tail) => iter(acc * head, tail)
      }
    }
    iter(1, this)
  }
  def double: IntList = {
    @tailrec
    def iter(acc: IntList, src: IntList, buffer: IntList): IntList = {
      (src, buffer) match {
        case (End, End)              => acc
        case (End, Pair(head, tail)) => iter(Pair(head, acc), End, tail)
        case (Pair(head, tail), buffer) =>
          iter(acc, tail, Pair(head * 2, buffer))
      }
    }
    iter(End, this, End)
  }
}
case object End extends IntList
final case class Pair(head: Int, tail: IntList) extends IntList

// 4.7.0.1 A Calculator AST
sealed trait Result
final case class Value(value: Double) extends Result
final case class Error(error: String) extends Result
// I prefer pattern matching in common case
sealed trait Expression {
  def eval: Result =
    this match {
      case Addition(left, right) =>
        left.eval match {
          case error: Error => error
          case Value(left) =>
            right.eval match {
              case error: Error => error
              case Value(right) => Value(left + right)
            }
        }
      case Subtraction(left, right) =>
        left.eval match {
          case error: Error => error
          case Value(left) =>
            right.eval match {
              case error: Error => error
              case Value(right) => Value(left - right)
            }
        }
      case Number(value) => Value(value)
      case Division(numerator, denominator) =>
        numerator.eval match {
          case error: Error => error
          case Value(nominator) =>
            denominator.eval match {
              case error: Error => error
              case Value(denominator) =>
                if (denominator == 0) Error("Division by zero")
                else Value(nominator / denominator)
            }
        }
      case SquareRoot(value) =>
        value.eval match {
          case error: Error => error
          case Value(value) =>
            if (value < 0) Error("Square root of negative number")
            else Value(math.sqrt(value))
        }
    }
}
final case class Addition(left: Expression, right: Expression)
    extends Expression
final case class Subtraction(left: Expression, right: Expression)
    extends Expression
final case class Number(value: Double) extends Expression
final case class Division(numerator: Expression, denominator: Expression)
    extends Expression
final case class SquareRoot(value: Expression) extends Expression
