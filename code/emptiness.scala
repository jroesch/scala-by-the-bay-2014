sealed trait Emptiness
trait Empty extends Emptiness
trait NonEmpty extends Emptiness

object simple {

}

object sstyle {
  sealed trait EList[+A] {
    type IsEmpty <: Emptiness
  }

  object EList {
    type Aux[A, E] = EList[A] { type IsEmpty = E }
  }

  case class Cons[+A](x: A, xs: EList[A]) extends EList[A] {
    type IsEmpty = NonEmpty
  }

  sealed trait ENil extends EList[Nothing] {
    type IsEmpty = Empty
  }

  case object ENil extends ENil

  def safeHead[A](xs: EList.Aux[A, NonEmpty]): A = xs match {
    case Cons(x, _) => x
  }

  def tryIt[A](xs: EList[A]): A = xs match {
    case me @ Cons(x, xs) => safeHead(me)
    case ENil => ???
  }
}
