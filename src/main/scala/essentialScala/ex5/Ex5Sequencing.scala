package essentialScala.ex5

import scala.annotation.tailrec

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
}
final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
final case class End[A]() extends LinkedList[A]

//  5.5.4.4 Sum
sealed trait Sum[A, B] {
  def fold[C](error: A => C, success: B => C): C =
    this match {
      case Failure(a) => error(a)
      case Success(b) => success(b)
    }
  def map[C](f: B => C): Sum[A, C] =
    this match {
      case Failure(a) => Failure(a)
      case Success(b) => Success(f(b))
    }
  def flatMap[C](f: B => Sum[A, C]): Sum[A, C] =
    this match {
      case Failure(a) => Failure(a)
      case Success(b) => f(b)
    }
}
final case class Failure[A, B](value: A) extends Sum[A, B]
final case class Success[A, B](value: B) extends Sum[A, B]

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
}
