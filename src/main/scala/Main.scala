import cats.effect.{ContextShift, IO}
import cats.implicits._
import shapeless._
import poly._

import scala.concurrent.ExecutionContext

object Main extends App {
  println("Hello")

  object choose extends (Set ~> Option) {
    def apply[T](s: Set[T]) = s.headOption
  }

  val sets = Set(1) :: Set("foo") :: HNil

  val opts = sets map choose

  println(opts)

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  //  Реализуйте функцию, чтобы она исполнялась максимально параллельно:
  def collectAll[A](f: A => IO[List[A]])(start: A): IO[List[A]] =
    f(start).flatMap { xs =>
      xs.parFlatTraverse(collectAll(f)).map(xs ++ _)
    }
}
