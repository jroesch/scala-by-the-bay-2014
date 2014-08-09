import shapeless._
import tag.@@
import shapeless.ops.hlist._
import ops.record._
import syntax.typeable._
import scala.reflect.runtime.universe._
import shapeless.tag._

trait SQLBacked[T] {
  type Repr <: HList
  type Keys <: HList
  type FieldNames <: HList
  
  val className: String
  val tableName: String
  val keys: Keys
  val fieldNames : List[String]

  def canUpdate[F <: HList](fields: F)(implicit subseq: SubSeq.Aux[F, Repr])
}

object SQLBacked {
  def apply[T](implicit sql: SQLBacked[T]): Aux[T, sql.Repr, sql.Keys, sql.FieldNames] = sql

  type Aux[T, R <: HList, K <: HList, F <: HList] = SQLBacked[T] { type Repr = R; type Keys = K; type FieldNames = F }
  
  // typeable stuff doesn't work, got to figure out how to stringify keys tomorrow */
  implicit def sqlbacked[T, R <: HList, O <: HList, I <: HList](implicit labelgen: LabelledGeneric.Aux[T, R], 
                                                                ks: Keys.Aux[R, O], 
                                                                mapper: Mapper.Aux[quote.type, O, I],
                                                                traversable: ToList[I, String],
                                                                ts: ToString[T]) = 
    new SQLBacked[T] {
      type Repr = R
      type Keys = O
      type FieldNames = I
      
      val className = ts.apply
      val tableName = ts.toTableName
      val keys = ks.apply
      val fieldNames = keys.map(quote).toList

      def canUpdate[F <: HList](fields: F)(implicit subseq: SubSeq.Aux[F, Repr]) = {
        ???
      }
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

trait Quotable[Q] {
  def quote(value: Q): String
}

object Quotable { //extends LowPriorityQuotable {
  def apply[Q](implicit q: Quotable[Q]) = q
  
  implicit object stringQuote extends Quotable[String] {
    def quote(value: String) = value
  }

  implicit def symbolQuote[T : Witness.Aux] = new Quotable[T] {
    def quote(value: T) = value.toString.substring(1)
  }
}

object quote extends Poly1 {
    implicit def default[Q](implicit quoter: Quotable[Q]) = at[Q](v => quoter.quote(v)) //quoter.quote(value)
}

object SQL {
 // import SQLBacked._
  //def using[T <: DB]

  
  def table[T](implicit sql: SQLBacked[T]): SQLBacked.Aux[T, sql.Repr, sql.Keys, sql.FieldNames] = sql
  
  def create[T](implicit sql: SQLBacked[T]) = {    
    s"CREATE TABLE ${table.tableName} (${sql.fieldNames.mkString(", ")});"
  }
  
  def insert[T: SQLBacked](row: T) = {
    s"INSERT INTO ${table.tableName} (${}) VALUES (${});"
  }

  def update[T: SQLBacked, F <: HList](fields: F) = ???

  def toTableName(name: String) = 
    name.map(_.toLower) + "s"
}

//sealed trait DB
//trait Postgres extends DB

case class User(name: String, lastname: String)

object Main {
  def main(args: Array[String]) {
    // SQL.create[User]
    // SQL.insert(User("Jared", "Roesch"))
  }
}
