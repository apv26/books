package essential_scala

import java.util.Date

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

  case class Circle(
    radius: Double
  ) extends Shape {
    val sides = 1
    val perimeter = math.Pi * 2 * radius
    val area = math.Pi * radius * radius
  }

  case class Rectangle(a: Double, b: Double) extends Shape {
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
  trait Visitor {
    def id: String
    def createdAt: Date
    def age: Long = new Date().getTime - createdAt.getTime
  }
  case class Anonymous(
    id: String,
    createdAt: Date = new Date()
  ) extends Visitor
  case class User(
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
  sealed trait Tree {}
  case class Node(left: Tree, right: Tree) extends Tree
  case class Leaf(element: Int) extends Tree
  object Math {
    def prettyPrint(t: Tree, indent: String): Unit = t match {
      case (t: Node) =>
        prettyPrint(t.left, indent + " ")
        prettyPrint(t.right, indent + " ")
        println(indent + ".")
      case (t: Leaf) => println(indent + t.element)
    }
    def sum(t: Tree): Int = t match {
      case (t: Node) => sum(t.left) + sum(t.right)
      case (t: Leaf) => t.element
    }
    def double(t: Tree): Tree = t match {
      case (t: Node) => Node(double(t.left), double(t.right))
      case (t: Leaf) => Node(Leaf(t.element), Leaf(t.element))
    }
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
  Math.prettyPrint(t, "")
  println(Math.sum(t))
  Math.prettyPrint(Math.double(t), "")

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
}
