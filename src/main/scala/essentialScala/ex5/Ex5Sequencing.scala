package essentialScala.ex5

import scala.annotation.tailrec

sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]

// 5.1.3.1 Generic List
// 5.1.3.2 Working With Generic Types
sealed trait LinkedList[A] {
  def length: Int = {
    @tailrec
    def iter(acc: Int, list: LinkedList[A]): Int = {
      list match {
        case End()         => acc
        case Pair(_, tail) => iter(acc + 1, tail)
      }
    }
    iter(0, this)
  }
  @tailrec
  def contains(el: A): Boolean = {
    this match {
      case End()            => false
      case Pair(head, tail) => if (head == el) true else tail.contains(el)
    }
  }
  def apply2(n: Int): A = {
    @tailrec
    def iter(acc: Int, list: LinkedList[A]): A = {
      list match {
        case End() =>
          throw new Exception("the nth item doesn't exists in the list")
        case Pair(head, _) if (acc == n) => head
        case Pair(_, tail)               => iter(acc + 1, tail)
      }
    }
    iter(0, this)
  }
  def apply(n: Int): Result[A] = {
    @tailrec
    def iter(acc: Int, list: LinkedList[A]): Result[A] = {
      list match {
        case End()                       => Failure("Index out of bounds")
        case Pair(head, _) if (acc == n) => Success(head)
        case Pair(_, tail)               => iter(acc + 1, tail)
      }
    }
    iter(0, this)
  }
}
final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
final case class End[A]() extends LinkedList[A]

// 5.3.4.1 Tree
sealed trait Tree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B
}
final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B =
    node(left.fold(node, leaf), right.fold(node, leaf))
}
final case class Leaf[A](value: A) extends Tree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B =
    leaf(value)
}

//  5.5.4.4 Sum
sealed trait Sum[A, B] {
  def fold[C](error: A => C, success: B => C): C =
    this match {
      case Failure1(a) => error(a)
      case Success1(b) => success(b)
    }
  def map[C](f: B => C): Sum[A, C] =
    this match {
      case Failure1(a) => Failure1(a)
      case Success1(b) => Success1(f(b))
    }
  def flatMap[C](f: B => Sum[A, C]): Sum[A, C] =
    this match {
      case Failure1(a) => Failure1(a)
      case Success1(b) => f(b)
    }
}
final case class Failure1[A, B](value: A) extends Sum[A, B]
final case class Success1[A, B](value: B) extends Sum[A, B]

object Ex5Sequencing extends App {

  val example = Pair(1, Pair(2, Pair(3, End())))
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End().length == 0)

  assert(example.contains(3) == true)
  assert(example.contains(4) == false)
  assert(End().contains(0) == false)
// This should not compile
// example.contains("not an Int")

  assert(example.apply2(0) == 1)
  assert(example.apply2(1) == 2)
  assert(example.apply2(2) == 3)
  assert(try {
    example.apply2(3)
    false
  } catch {
    case e: Exception => true
  })

  assert(example(0) == Success(1))
  assert(example(1) == Success(2))
  assert(example(2) == Success(3))
  assert(example(3) == Failure("Index out of bounds"))

  // 5.3.4.1 Tree
  val tree: Tree[String] =
    Node(
      Node(Leaf("To"), Leaf("iterate")),
      Node(
        Node(Leaf("is"), Leaf("human,")),
        Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))
      )
    )
  System.out.println(tree.fold[String]((a, b) => a + " " + b, str => str))
}
