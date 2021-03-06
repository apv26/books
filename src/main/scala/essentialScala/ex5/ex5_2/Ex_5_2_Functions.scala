package essentialScala.ex5.ex5_2

sealed trait IntList {
  def length: Int =
    this match {
      case End          => 0
      case Pair(hd, tl) => 1 + tl.length
    }
  def double: IntList =
    this match {
      case End          => End
      case Pair(hd, tl) => Pair(hd * 2, tl.double)
    }
  def product: Int =
    this match {
      case End          => 1
      case Pair(hd, tl) => hd * tl.product
    }
  def sum: Int =
    this match {
      case End          => 0
      case Pair(hd, tl) => hd + tl.sum
    }
  // 5.2.3.1 A Better Abstraction
  def fold(end: Int, f: (Int, Int) => Int): Int =
    this match {
      case End          => end
      case Pair(hd, tl) => f(hd, tl.fold(end, f))
    }
}
case object End extends IntList
final case class Pair(hd: Int, tl: IntList) extends IntList
