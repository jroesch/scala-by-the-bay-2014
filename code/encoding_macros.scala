package encoding.macros

import java.nio._
import charset._
import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import shapeless._

/* A set of example encodings */
sealed trait Encoding 
sealed trait UTF8 extends Encoding 
sealed trait ASCII extends Encoding
sealed trait Binary extends Encoding

object Encoding {
  def encodingNameForType[E <: Encoding : WeakTypeTag]: String = 
    weakTypeTag[E].tpe.toString
}

object encodingName extends Poly0 {
  implicit def ascii[E <: ASCII] = at[String]("ASCII")
}

/* Inteface for an Encoded String */
trait EncodedString[E <: Encoding] {
  val contents: Array[Byte]
  override def toString = new String(contents, Encoding.encodingNameForType[E])
}

/* Macro for building encoded literals */
class StaticEncoding(val c: Context) {
  def encnameForType[E <: Encoding : c.WeakTypeTag]: String = 
    weakTypeTag[E].tpe.toString

  def encodeAs[E <: Encoding : c.WeakTypeTag](s : c.Expr[String]): c.Expr[EncodedString[E]] = {
    import c.universe._
    s.tree match {
      case Literal(Constant(literal: String)) =>
        validInEncoding(encoding, literal) match {
          case true =>
            val etpe = weakTypeTag[E].tpe
            val tree = q"""new EncodedString[$etpe] {
              def encodingName = Encoding.encodingNameForType[$etpe]
              val contents = $literal.getBytes
            }"""
            c.Expr[EncodedString[E]](tree)
          case _ => 
            c.abort(c.enclosingPosition, "The string can not be transcoded")
        }
      case _ => 
        c.abort(c.enclosingPosition, "You must pass a string literal to assert its encoding statically.")
    }
  }

  def validInEncoding(encoding: String, s: String): Boolean = {
    val charset = Charset.forName(encoding)
    val charBuffer = CharBuffer.wrap(s)
    val encoder = charset.newEncoder
    try {
      encoder.encode(charBuffer)
      true
    } catch {
      case _: UnmappableCharacterException => false
    }
  }

  def getEncoding(name: String): String = name match {
      case "encoding.macros.UTF8" => "UTF-8"
      case "encoding.macros.ASCII" => "ASCII"
      case "encoding.macros.Binary" => "ISO-8859-1"
  }
}

object StringEncodingMacros {
  def staticEncoding[E <: Encoding](s: String) = macro StaticEncoding.encodeAs[E]
}

object Transcode {
  type Aux[E <: Encoding, R <: Encoding] = Transcode[E] { type Result = R }
  
  implicit def identityTranscode[E <: Encoding] = new Transcode[E] {
    type Result = E
    def transcode(s: EncodedString[E]): EncodedString[E] = s
  }

  implicit def asciiToUtf8[E <: ASCII, R <: UTF8] = new Transcode[E] {
    type Result = R
    
    def transcode(s: EncodedString[E]) = new EncodedString[Result] {
      val contents = s.contents
    }
  }
}

trait Transcode[Initial <: Encoding] {
  type Result <: Encoding

  def transcode(s: EncodedString[Initial]): EncodedString[Result]
}

object Concatable {
  type Aux[P <: Encoding, S <: Encoding, R  <: Encoding] = Concatable[P, S] { type Result = R }

  def apply[Prefix <: Encoding, Suffix <: Encoding](implicit c: Concatable[Prefix, Suffix]) : Aux[Prefix, Suffix, c.Result] = c
  
  implicit def concatable[E1 <: Encoding, E2 <: Encoding, R <: Encoding]
  (implicit t1: Transcode.Aux[E1, R], t2: Transcode.Aux[E2, R]) = new Concatable[E1, E2] {
    type Result = R
    def concat(s1: EncodedString[E1], s2: EncodedString[E2]) = {
      val s1p = t1.transcode(s1)
      val s2p = t2.transcode(s2)
      new EncodedString[R] {
        val contents = s1p.contents ++ s2p.contents
      }
    }
  }
}

trait Concatable[Prefix <: Encoding, Suffix <: Encoding] {
  type Result <: Encoding

  def concat(s1: EncodedString[Prefix], s2: EncodedString[Suffix]): EncodedString[Result]
}


