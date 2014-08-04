sealed trait Encoding
sealed trait UTF8 extends Encoding
case object UTF8 extends UTF8

sealed trait ASCII extends Encoding
case object ASCII extends ASCII

/* Types are contracts */
sealed trait EncodedString[E <: Encoding]
case class UTF8String(s: String) extends EncodedString[UTF8]
case class ASCIIString(s: String) extends EncodedString[ASCII]

