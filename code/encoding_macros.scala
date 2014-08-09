import java.nio._
import charset._
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

/* A set of example encodings */
sealed trait Encoding {
  val encodingName: String
}

sealed trait UTF8 extends Encoding
case object UTF8 extends UTF8 {
  val encodingName = "UTF-8"
}

sealed trait ASCII extends Encoding
case object ASCII extends ASCII {
  val encodingName = "ASCII"
}

sealed trait Binary extends Encoding
case object Binary extends Binary {
  val encodingName = "ISO-8859-1"
}

/* Inteface for an Encoded String */
trait EncodedString[E <: Encoding] {
  val encoding: String
  val contents: Array[Byte]
  override def toString = new String(contents, encoding)

  def concat(other: EncodedString[E]): EncodedString[E] = {
    val that = this
    new EncodedString[E] {
      val encoding = that.encoding
      val contents = that.contents ++ other.contents
    }
  }
}

/* Macro for building encoded literals */
class StaticEncoding(val c: Context) {
  def encodeAs[E <: Encoding : c.WeakTypeTag](enc: c.Expr[E], s: c.Expr[String]): c.Expr[EncodedString[E]] = {
    import c.universe._
    s.tree match {
      case Literal(Constant(literal: String)) =>
        val encoding = getEncoding(enc.tree)
        val encodingName = encoding.encodingName
        validInEncoding(encoding, literal) match {
          case true =>
            val etpe = weakTypeTag[E].tpe
            val tree = q"""new EncodedString[$etpe] {
              val encoding = $encodingName
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

  def validInEncoding(encoding: Encoding, s: String): Boolean = {
    val charset = Charset.forName(encoding.encodingName)
    val charBuffer = CharBuffer.wrap(s)
    val encoder = charset.newEncoder
    try {
      encoder.encode(charBuffer)
      true
    } catch {
      case _: UnmappableCharacterException => false
    }
  }

  def getEncoding(name: c.Tree): Encoding = {
    import c.universe._

    name match {
      case q"UTF8" => UTF8
      case q"ASCII" => ASCII
      case q"Binary" => Binary
    }
  }
}

object StringEncodingMacros {
  def staticEncoding[E <: Encoding](enc: E, s: String) = macro StaticEncoding.encodeAs[E]
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
      val encoding = "UTF-8"
      val contents = s.contents
    }
  }
}

trait Transcode[Initial <: Encoding] {
  type Result <: Encoding

  def transcode(s: EncodedString[Initial]): EncodedString[Result]
}

object Concatable {
  implicit def concatable[E1 <: Encoding, E2 <: Encoding, R <: Encoding]
  (implicit t1: Transcode.Aux[E1, R], t2: Transcode.Aux[E2, R]) = new Concatable[E1, E2] {
    type Result = R
    def concat(s1: EncodedString[E1], s2: EncodedString[E2]) = {
      val s1p = t1.transcode(s1)
      val s2p = t2.transcode(s2)
      s1p.concat(s2p)
    }
  }
}

trait Concatable[Prefix <: Encoding, Suffix <: Encoding] {
  type Result <: Encoding

  def concat(s1: EncodedString[Prefix], s2: EncodedString[Suffix]): EncodedString[Result]
}

object enc {
  def concat[E1 <: Encoding, E2 <: Encoding](s1: EncodedString[E1], s2: EncodedString[E2])(implicit c: Concatable[E1, E2]) =
    c.concat(s1, s2)
}

