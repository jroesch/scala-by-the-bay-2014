import shapeless._
import ops.hlist.{Mapper, Unifier}
import poly._
import scala.reflect.runtime.universe._

object stringFromType extends Poly1 {
  implicit def default[T: WeakTypeTag] = at[T](t => weakTypeTag[T].tpe.toString)
}

trait StringRepr[T] {
  def apply: String
}

object StringRepr {
  implicit def stringRepr[T: WeakTypeTag] = new StringRepr[T] {
    def apply = weakTypeTag[T].tpe.toString
  }
}


trait TypeToString[H <: HList] {
  def apply: List[String]
}

object TypeToString {
  implicit def hnilTypeToString = new TypeToString[HNil] {
    def apply = Nil
  }

  implicit def hconsTypeToString[H, T <: HList](implicit sr: StringRepr[H], tts: TypeToString[T]) = new TypeToString[H :: T] {
    def apply = sr.apply :: tts.apply
  }
}

object Example {
  def typeToString[H <: HList](implicit tts: TypeToString[H]) = tts.apply.mkString("(", ", ", ")")
}
