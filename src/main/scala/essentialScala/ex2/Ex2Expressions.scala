package essentialScala.ex2

object Ex2Expressions extends App {

  // 2.2.5.1 OperatorStyle
  "foo".take(1)
  "foo" take 1
  1.+(2).+(3)

  // 2.3.8.1 LiterallyJustLiterals
  // Int Boolean Long Double

  // 2.3.8.2 Quotes and Misquotes
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
    def square(l: Double): Double = l * l

    def cube(l: Double): Double = l * square(l)
  }

  // 2.4.5.3 Precise Square Dance!
  object calc2 {
    def square(l: Double) = l * l
    def cube(l: Double) = l * square(l)
    def square(l: Int) = l * l
    def cube(l: Int) = l * square(l)
  }

  // 2.4.5.4 Order of evaluation
  // a = 1, b = 3, c = "3c"
  // 3c31

  // 2.4.5.5 Greetings, human
  object person {
    val firstName = ""
    val lastName = ""
  }

  object alien {
    def greet(p: person.type): String = "Greetings, " + p.firstName

    // No
  }

  // 2.4.5.6 The Value of Methods
  object calculator {
    def square(x: Int) = x * x
  }
  // only expression or value can be assigned to a field
  // val someField = calculator.square
  // gives compile error: missing argument list for method square in object calculator,
  // so methods are not values nor expressions
  val someField = calculator.square _
  // functions are objects and functions are also values
  // calls of methods yields values

  //  2.6.4.1 A Classic Rivalry
  // type - String, value - "predator"
  // 2.6.4.2 A Less Well Known Rivalry
  // type - Any, value - 2001
  // 2.6.4.3 An if Without an else
  // type - Any, value - ()
}
