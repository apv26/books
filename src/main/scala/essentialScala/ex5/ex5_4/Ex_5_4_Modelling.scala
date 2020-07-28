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
sealed trait Sum[A, B]
final case class Left[A, B](value: A) extends Sum[A, B]
final case class Right[A, B](value: B) extends Sum[A, B]

// 5.4.4.1 Exercise: Maybe that Was a Mistake
sealed trait Maybe[A]
final case class Empty[A]() extends Maybe[A]
final case class Full[A](value: A) extends Maybe[A]
