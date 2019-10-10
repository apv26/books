package essential_scala

import essential_scala.Ex3Classes.Adder

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

  val eastwood = new Director("Clint", "Eastwood", 1930)
  val mcTiernan = new Director("John", "McTiernan", 1951)
  val nolan = new Director("Christopher", "Nolan", 1970)
  val someBody = new Director("Just", "SomeBody", 1990)
  val memento = new Film("Memento", 2000, 8.5, nolan)
  val darkKnight = new Film("DarkKnight", 2008, 9.0, nolan)
  val inception = new Film("Inception", 2010, 8.8, nolan)
  val highPlainsDrifter = new Film("HighPlainsDrifter", 1973, 7.7, eastwood)
  val outlawJoseyWales = new Film("TheOutlawJoseyWales", 1976, 7.9, eastwood)
  val unforgiven = new Film("Unforgiven", 1992, 8.3, eastwood)
  val granTorino = new Film("GranTorino", 2008, 8.2, eastwood)
  val invictus = new Film("Invictus", 2009, 7.4, eastwood)
  val predator = new Film("Predator", 1987, 7.9, mcTiernan)
  val dieHard = new Film("DieHard", 1988, 8.3, mcTiernan)
  val huntForRedOctober = new Film("TheHuntforRedOctober", 1990, 7.6, mcTiernan)
  val thomasCrownAffair = new Film("TheThomasCrownAffair", 1999, 6.8, mcTiernan)
  println(eastwood.yearOfBirth)
  println(dieHard.director.name)
  println(invictus.isDirectedBy(nolan))

  println(highPlainsDrifter.copy(name = "L'homme des hautes plaines"))
  println(
    thomasCrownAffair.copy(yearOfRealease = 1968,
                           director = new Director("Norman", "Jewison", 1926)))
  println(inception.copy().copy().copy())

  // 3.1.6.4 A Simple Counter
  // 3.1.6.6 Additional Counting
  class Adder(amount: Int) {
    def add(in: Int) = in + amount
  }
  class Counter(val count: Int) {
    def inc = new Counter(count + 1)
    def dec = new Counter(count - 1)
    def adjust(adder: Adder): Counter = {
      new Counter(adder.add(count))
    }
  }
  println(new Counter(10).inc.dec.inc.inc.count)
  println(new Counter(10).adjust(new Adder(4)).count)
  // 3.1.6.5 Counting Faster
  class Counter1(val count: Int) {
    def inc: Counter1 = inc()
    def dec: Counter1 = dec()
    def inc(v: Int = 1): Counter1 = new Counter1(count + v)
    def dec(v: Int = 1): Counter1 = new Counter1(count - v)
  }
  println(new Counter1(10).inc.inc(10).count)

  // 3.2.3.1 When is a Function is not a Function?
  // other developers need to know about our specific class to use it

  //  3.4.5.1 Case Cats
  case class Cat2(
    colour: String,
    food: String
  )

  // 3.3.2.1 Friendly Person Factory
  println(Person("James Bond"))
  println(Person("Chingachgook The Great Snake"))
  println(Person("Chingachgook"))
  println(Person(""))

  // 3.4.5.3 Case Class Counter
  println(Counter2(0))
  println(Counter2().inc)
  println(Counter2().inc.dec == Counter2().inc.dec)

  // 3.4.5.4 Application, Application, Application
  println(Person1("James Bond"))
  println(Person1("Chingachgook The Great Snake"))
  println(Person1("Chingachgook"))
  println(Person1(""))
}

// 3.3.2.1 Friendly Person Factory
class Person(firstName: String, lastName: String) {
  override def toString: String = s"$firstName $lastName"
}
object Person {
  def apply(name: String): Person = {
    name.split(" ").toList match {
      case f :: l => new Person(f, l.mkString(" "))
      case Nil => Person("")
    }
  }
}

// 3.1.6.3 DirectorialDebut
class Director(
  val firstName: String,
  val lastName: String,
  val yearOfBirth: Int
) {
  def name: String = s"$firstName$lastName"
}
// 3.3.2.2 Extended Body of Work
object Director {
  def apply(firstName: String, lastName: String, yearOfBirth: Int): Director =
    new Director(firstName, lastName, yearOfBirth)
  def older(director1: Director, director2: Director): Director =
    if (director1.yearOfBirth < director2.yearOfBirth) director1 else director2
}
class Film(
  val name: String,
  val yearOfRealease: Int,
  val imbdRating: Double,
  val director: Director
) {
  def directorAge = yearOfRealease - director.yearOfBirth
  def isDirectedBy(director: Director) = this.director == director
  def copy(
    name: String = this.name,
    yearOfRealease: Int = this.yearOfRealease,
    imbdRating: Double = this.imbdRating,
    director: Director = this.director
  ): Film = new Film(name, yearOfRealease, imbdRating, director)
}
object Film {
  def apply(
    name: String,
    yearOfRealease: Int,
    imbdRating: Double,
    director: Director): Film =
    new Film(name, yearOfRealease, imbdRating, director)
  def highestRating(film1: Film, film2: Film): Double = {
    val rating1 = film1.imbdRating
    val rating2 = film2.imbdRating
    if (rating1 >= rating2) rating1 else rating2
  }
  def oldestDirectorAtTheTime(film1: Film, film2: Film): Director = {
    if (film1.directorAge > film2.directorAge) film1.director else film2.director
  }
}

  // 3.3.2.3 Type or Value?
  // val prestige: Film = bestFilmByChristopherNolan()
  // type
  // new Film("LastActionHero", 1993, mcTiernan)
  // type
  // Film("LastActionHero", 1993, mcTiernan)
  // value
  // Film.newer(highPlainsDrifter, thomasCrownAffair)
  // value
  // Film.type
  // value

// 3.4.5.2 Roger Ebert Said it Best...
case class Director1(
  firstName: String,
  lastName: String,
  yearOfBirth: Int
) {
  def name: String = s"$firstName$lastName"
}
object Director1 {
  def older(director1: Director1, director2: Director1): Director1 =
    if (director1.yearOfBirth < director2.yearOfBirth) director1 else director2
}
case class Film1(
  name: String,
  yearOfRealease: Int,
  imbdRating: Double,
  director: Director1
) {
  def directorAge = yearOfRealease - director.yearOfBirth
  def isDirectedBy(director: Director1) = this.director == director
}
object Film1 {
  def highestRating(film1: Film1, film2: Film1): Double = {
    val rating1 = film1.imbdRating
    val rating2 = film2.imbdRating
    if (rating1 >= rating2) rating1 else rating2
  }
  def oldestDirectorAtTheTime(film1: Film1, film2: Film1): Director1 = {
    if (film1.directorAge > film2.directorAge) film1.director else film2.director
  }
}
// case class features
// a field for each constructor argument — we don’t even need to write val
// copy method
// apply methods - create Instance without new
// provides equals methods, toString methods, and pattern matching

// 3.4.5.3 Case Class Counter
case class Counter2(count: Int = 0) {
  def dec = copy(count = count - 1)
  def inc = copy(count = count + 1)
}

// 3.4.5.4 Application, Application, Application
case class Person1(firstName: String, lastName: String)
object Person1 {
  def apply(name: String): Person1 = {
    name.split(" ").toList match {
      case f :: l => new Person1(f, l.mkString(" "))
      case Nil => Person1("")
    }
  }
}

// 3.5.3.2 Get Off My Lawn!
object Dad {
  def rate(film: Film1): Double = film match {
    case Film1(_, _, _, Director1("Clint", "Eastwood", _)) => 10.0
    case Film1(_, _, _, Director1("John", "McTiernan", _)) => 7.0
    case _ => 3.0
  }
}