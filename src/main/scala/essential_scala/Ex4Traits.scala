package essential_scala

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
}
