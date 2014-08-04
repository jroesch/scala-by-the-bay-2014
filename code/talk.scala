object talk {
  import shapeless._

  /* 
  Shapeless is about exploring what is possible in Scala.
  Miles and contributors have spent a lot of time strechting the boundaries of what is possible to do in Scala
  The core idea is to bring expressive tools of Generic and Depdently typed programming to Scala.
  It began with the ideas from work done on Generic Programming in Haskell called ScrapYourBoiler plate.
  Shapeless can look very complicated from the perspective of the normal Scala programmer, but it is
  actually a powerful tool encoding simple ideas, albeit it in some complicated ways.

  shapeless allows us to do this pretty easily by exploiting a few different Scala features.
  type memebers, Path dependent types, implicits

  For example to bring depdent function types, where the input type can determine the out
  */

  /* We want to be able to encode proofs about types. */
  trait LessThan[N <: Nat, M <: Nat]
  
  object LessThan {
    implicit def lessThan0[M <: Nat]: LessThan[_0, Succ[M]] = new LessThan[_0, Succ[M]] {}
    implicit def lessThan[N <: Nat, M <: Nat](implicit proof: LessThan[N, M]) : LessThan[Succ[N], Succ[M]] = 
      new LessThan[Succ[N], Succ[M]] {}
  }

  def staticLessThan(n: Nat, m: Nat)(implicit proof: LessThan[n.N, m.N]) = true

  sealed trait Vect[+A] {
    type Size <: Nat
  }

  object Vect {
    type Aux[N <: Nat, +A] = Vect[A] { type Size = N }
  }
  
  sealed trait VNil extends Vect[Nothing] {
    type Size = _0
  }

  case object VNil extends VNil
  
  case class VCons[N <: Nat, A](head: A, tail: Vect.Aux[N, A]) extends Vect[A] {
    type Size = Succ[N]
  }

  def onlyLessThanN[A, V[_] <: Vect[_], M <: Nat](n: Nat, v: Vect.Aux[M, A])(implicit lessThan: LessThan[M, n.N]) = v
}
