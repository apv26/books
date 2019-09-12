package essential_scala

object Ex extends App {

  // 2.2.5.1 OperatorStyle
  "foo".take(1)
  "foo" take 1
    1.+(2).+(3)

  // 2.3.8.1 LiterallyJustLiterals
  // Int Boolean Long Double

  // 2.3.8.2 Quotesand Misquotes
  // Character String

  // 2.3.8.3 AnAside on Side-Effects
  // String Unit

//  2.4.5.1 Cat_o_matique

  object Cat1 {
    val name = "Oswald"
    val colour = "Black"
    val food = "Milk"
  }

  object Cat2 {
    val name = "Henderson"
    val colour = "Ginger"
    val food = "Chips"
  }

  object Cat3 {
    val name = "Quentin"
    val colour = "Tabby and white"
    val food = "Curry"
  }

// 2.4.5.2 Square Dance!
  object calc {
    def square(l: Double): Double = l*l
    def cube(l: Double): Double = l*square(l)
  }

// 2.4.5.4 Order of evaluation
  // a = 1, b = 3, c = "3c"
  // 3c31

// 2.4.5.5 Gree?ngs,human
  object person {
    val firstName = ""
    val lastName = ""
  }

  object alien {
    def greet(p: person.type): String = "Greetings, " + p.firstName
    // No
  }

//  3.1.6.1 Cats,Again
  class Cat1(
    val colour: String,
    val food: String
  )
  val oswald = new Cat1("Black", "Milk")
  val henderson = new Cat1("Ginger", "Chips")
  val quentin = new Cat1("Tabby and white", "Curry")

//  3.1.6.2 Catson the Prowl
  object ChipShop {
    def willServe(cat: Cat1) = cat.food == "Chips"
  }
//  3.4.5.1 Case Cats
case class Cat2(
  colour: String,
  food: String
)

//  4.1.4.1 Cats,and MoreCats
  trait Feline {
    val colour: String
    val sound: String
  }
  case class Tiger (colour: String, maneSize: Int) extends Feline {
    val sound = "roar"
  }
  case class Lion (colour: String) extends Feline {
    val sound: String = "roar"
  }
  case class Panther (colour: String) extends Feline {
    val sound: String = "roar"
  }
  case class Cat (colour: String, food: String) extends Feline {
    val sound: String = "meow"
  }

  // 4.1.4.2 ShapingUpWithTraits
  trait Shape {
    def sides: Int
    def perimeter: Double
    def area: Double
  }
  case class Circle (
    radius: Double
  ) extends Shape {
    val sides = 1
    val perimeter = math.Pi * 2 * radius
    val area = math.Pi * radius * radius
  }
  case class Rectangle (
   a: Double,
   b: Double
  ) extends Shape {
    val sides = 4
    val perimeter = 2 * (a + b)
    val area = a * b
  }
  case class Square (
    a: Double
  ) extends Shape {
    val sides = 4
    val perimeter = 4 * a
    val area = a * a
  }
//  4.5.6.3 Email
}