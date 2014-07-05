import shapeless._
import scala.reflect.runtime.universe._

trait SQLBacked[T]

object SQLBacked {
  def apply[T](implicit sql: SQLBacked[T]): SQLBacked[T] = sql
  
  implicit def sqlbacked[T](implicit labelgen: LabelledGeneric[T]): SQLBacked[T] = new SQLBacked[T] {}
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

object SQL {
 // import SQLBacked._
  //def using[T <: DB] 
  def create[T: SQLBacked](implicit ts: ToString[T]): String = {
    val tableName = ts.toTableName
    s"CREATE TABLE $tableName ();"
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
