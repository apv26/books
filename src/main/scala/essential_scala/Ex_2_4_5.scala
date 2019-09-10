package essential_scala

object Ex_2_4_5 extends App {

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
}