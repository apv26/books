package essentialScala.ex5.ex5_2

sealed trait IntList {
  // def length: Int =
  //   this match {
  //     case End          => 0
  //     case Pair(hd, tl) => 1 + tl.length
  //   }
  // def double: IntList =
  //   this match {
  //     case End          => End
  //     case Pair(hd, tl) => Pair(hd * 2, tl.double)
  //   }
  // def product: Int =
  //   this match {
  //     case End          => 1
  //     case Pair(hd, tl) => hd * tl.product
  //   }
  // def sum: Int =
  //   this match {
  //     case End          => 0
  //     case Pair(hd, tl) => hd + tl.sum
  //   }
  // 5.2.3.1 A Better Abstraction Part 1
  def fold[A](end: A, f: (Int, A) => A): A =
    this match {
      case End          => end
      case Pair(hd, tl) => f(hd, tl.fold(end, f))
    }

  // 5.2.3.1 A Better Abstraction Part 2
  def length: Int = fold(0, (_, tl: Int) => 1 + tl)
  // 5.2.3.1 A Better Abstraction Part 5
  def double: IntList = fold(End, (hd: Int, tl: IntList) => Pair(hd * 2, tl))
  def product: Int = fold(1, (hd: Int, tl: Int) => hd * tl)
  def sum: Int = fold(0, (hd: Int, tl: Int) => hd + tl)

  // 5.2.3.1 A Better Abstraction Part 3
  // When using fold in polymorphic implementations we have a lot of duplica-
  // tion; the polymorphic implementations without fold were simpler to write.
  // The pattern matching implementations benefiteed from fold as we removed
  // the duplication in the pattern matching.
  // In general fold makes a good interface for users outside the class, but not
  // necessarily for use inside the class.
}
case object End extends IntList
final case class Pair(hd: Int, tl: IntList) extends IntList
