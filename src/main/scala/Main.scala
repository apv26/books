import shapeless._
import poly._

object Main extends App {
  println("Hello")

  object choose extends (Set ~> Option) {
    def apply[T](s: Set[T]) = s.headOption
  }

  val sets = Set(1) :: Set("foo") :: HNil

  val opts = sets map choose

  println(opts)
}