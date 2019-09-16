package essential_scala

object Ex3Classes extends App {

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
}
