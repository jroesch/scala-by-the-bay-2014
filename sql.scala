import shapeless._
import record._
import shapeless.ops.hlist._
import ops.record._
import syntax.typeable._
import scala.reflect.runtime.universe._

trait SQLBacked[T] {
  type Repr <: HList
  type Keys <: HList
  
  val className: String
  val tableName: String
  val keys: Keys
}

object SQLBacked {
  def apply[T](implicit sql: SQLBacked[T]): Aux[T, sql.Repr, sql.Keys] = sql

  type Aux[T, R <: HList, K <: HList] = SQLBacked[T] { type Repr = R; type Keys = K }
  
  // typeable stuff doesn't work, got to figure out how to stringify keys tomorrow */
  implicit def sqlbacked[T, R <: HList, O <: HList, C](implicit labelgen: LabelledGeneric.Aux[T, R], ks: Keys.Aux[R, O], tta: ToList[O, C], typ: Typeable[C], ts: ToString[T]) = 
    new SQLBacked[T] {
      type Repr = R
      type Keys = O
      
      val className = ts.apply
      val tableName = ts.toTableName
      val keys = ks.apply
    }
}

trait ToString[T] {
  def apply: String

  def toTableName = 
    this.apply.map(_.toLower) + "s"
}

object ToString {
  def apply[T](implicit ts: ToString[T]) = ts.apply

  implicit def stringRepr[T: WeakTypeTag] = new ToString[T] {
    def apply = weakTypeTag[T].tpe.toString
  }
}

trait SubSeq[L <: HList] {
  type Sub <: HList
}

trait LowPrioritySubSeq {
  implicit def hconsSubSeq[H, T <: HList, SH, ST <: HList](implicit subseq: SubSeq.Aux[T, SH :: ST]): SubSeq.Aux[H :: T, SH :: ST] = new SubSeq[H :: T] { type Sub = SH :: ST }
}

object SubSeq extends LowPrioritySubSeq {
  def apply[L <: HList, S <: HList](implicit subseq: SubSeq.Aux[L, S]) = subseq
  
  type Aux[L <: HList, S <: HList] = SubSeq[L] { type Sub = S }
  
  implicit def hnilSubSeq: SubSeq.Aux[HNil, HNil] = new SubSeq[HNil] { type Sub = HNil }
  implicit def hconsSubSeqIso[H, SH, T <: HList, ST <: HList](implicit iso: H =:= SH, subseq: SubSeq.Aux[T, ST]): SubSeq.Aux[H :: T, SH :: ST] = new SubSeq[H :: T] { type Sub = SH :: ST }
}

object SQL {
 // import SQLBacked._
  //def using[T <: DB]
  def table[T](implicit sql: SQLBacked[T]): SQLBacked.Aux[T, sql.Repr, sql.Keys] = sql
  
  def create[T: SQLBacked] = {
    val table = SQL.table[T]
    s"CREATE TABLE ${table.tableName} (${table.keys.toList.cast[List[Symbol]].get});" //yolo!
    table.keys
  }
  
  def insert[T: SQLBacked](row: T) = ???

  def toTableName(name: String) = 
    name.map(_.toLower) + "s"
}

//sealed trait DB
//trait Postgres extends DB

case class User(name: String, lastname: String)

object Main {
  def main(args: Array[String]) {
    SQL.create[User]
    SQL.insert(User("Jared", "Roesch"))
  }
}
