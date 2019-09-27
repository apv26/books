package essential_scala

object Ex5Sequencing extends App {

  //  5.5.4.4 Sum
  sealed trait Sum[A, B] {
    def fold[C](error: A => C, success: B => C): C =
      this match {
        case Failure(a) => error(a)
        case Success(b) => success(b)
      }
    def map[C](f: B => C): Sum[A, C] =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => Success(f(b))
      }
    def flatMap[C](f: B => Sum[A, C]): Sum[A, C] =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => f(b)
      }
  }
  final case class Failure[A, B](value: A) extends Sum[A, B]
  final case class Success[A, B](value: B) extends Sum[A, B]
}
