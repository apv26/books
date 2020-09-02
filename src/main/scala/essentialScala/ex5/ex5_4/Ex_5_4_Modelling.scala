package essentialScala.ex5.ex5_4

object Ex_5_4_Modelling extends App {

// 5.4.1.1 Exercise: Pairs
  val pair = Pair[String, Int]("hi", 2)
// pair: Pair[String,Int] = Pair(hi,2)
  System.out.println(pair.one)
// res0: String = hi
  pair.two
// res1: Int = 2

// 5.4.3.1 Exercise: Generic Sum Type
  System.out.println("5.4.3.1 Exercise: Generic Sum Type")
  System.out.println(Left[Int, String](1).value)
// res9: Int = 1
  System.out.println(Right[Int, String]("foo").value)
// res10: String = foo
  val sum: Sum[Int, String] = Right("foo")

  System.out.println(sum match {
    case Left(x)  => x.toString
    case Right(x) => x
  })
// res11: String = foo

// 5.4.4.1 Exercise: Maybe that Was a Mistake
  System.out.println("5.4.4.1 Exercise: Maybe that Was a Mistake")
  val perhaps: Maybe[Int] = Empty[Int]
  val perhaps1: Maybe[Int] = Full(1)
}

// 5.4.1.1 Exercise: Pairs
final case class Pair[A, B](one: A, two: B)

// 5.4.3.1 Exercise: Generic Sum Type
sealed trait Sum[A, B] {
  // 5.4.6.3 Folding Sum
  def fold[C](left: A => C, right: B => C): C = {
    this match {
      case Left(v)  => left(v)
      case Right(v) => right(v)
    }
  }
}
final case class Left[A, B](value: A) extends Sum[A, B]
final case class Right[A, B](value: B) extends Sum[A, B]

// 5.4.4.1 Exercise: Maybe that Was a Mistake
// 5.4.6.2 Folding Maybe
// you can choose pattern matching in the base trait for the solution
sealed trait Maybe[A] {
  def fold[B](full: A => B, empty: B): B =
    this match {
      case Full(v) => full(v)
      case Empty() => empty
    }
}
final case class Empty[A]() extends Maybe[A]
final case class Full[A](value: A) extends Maybe[A]

// 5.4.6.1 Generics versus Traits
// Ultimately the decision is up to us. Different teams will adopt different programming
// styles. However, we look at the properties of each approach to
// inform our choices:
// Inheritance-based approaches—traits and classes—allow us to create permanent
// data structures with specific types and names. We can name every field
// and method and implement use-case-specific code in each class. Inheritance
// is therefore better suited to modelling significant aspects of our programs that
// are re-used in many areas of our codebase.
// Generic data structures—Tuples, Options, Eithers, and so on—are extremely
// broad and general purpose. There are a wide range of predefined
// classes in the Scala standard library that we can use to quickly model rela-
// tionships between data in our code. These classes are therefore better suited
// to quick, one-off pieces of data manipulation where defining our own types
// would introduce unnecessary verbosity to our codebase.
