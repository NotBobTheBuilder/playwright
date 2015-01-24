package playwright.lang

import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps

trait Transpilable

class Type
object TNumber extends Type
object TNull extends Type
object TUndefined extends Type
object TString extends Type
object TFunction extends Type
object TInferred extends Type

object Type {
  def withName(n: String) = {
    n match {
      case "Number" => TNumber
      case "Null" => TNull
      case "Undefined" => TUndefined
      case "String" => TString
      case "Function" => TFunction
    }
  }
}

case class Param (
  name: String,
  T: Type
) extends Transpilable

abstract class Expr extends Transpilable

case class Statement (
  op: Expr
) extends Transpilable

case class Return (
  expr: Expr
) extends Transpilable

case class FunctionSignature (
  params: List[Param],
  T: Type
) extends Transpilable

case class FunctionBody (
  stats: List[Statement],
  retval: Expr
) extends Transpilable

case class Function (
  sig: FunctionSignature,
  body: FunctionBody,
  T: Type
) extends Expr with Transpilable

case class Number (
  v: Double
) extends Expr with Transpilable

object PlaywrightParser extends RegexParsers {
  def ident:          Parser[String]              = """\w+""".r
  def typeIdent:      Parser[Type]                = ident ^^ Type.withName
  def param:          Parser[Param]               = ident ~ (":" ~> typeIdent)  ^^ { case p ~ t => new Param(p, t)}

  def paramList:      Parser[List[Param]]         = param ~ (("," ~> param)*)   ^^ { case p ~ ps => p :: ps }

  def funcSignature:  Parser[FunctionSignature]   = (("(" ~> (paramList) <~ ")")?) ~ ((":" ~> typeIdent)?) ^^ {
    case Some(ps) ~ Some(t)   => new FunctionSignature(ps, t)
    case Some(ps) ~ None      => new FunctionSignature(ps, TInferred)
    case None ~ Some(t)       => new FunctionSignature(List(), t)
    case None ~ None          => new FunctionSignature(List(), TInferred)
  }
  def funcBody:       Parser[FunctionBody]        = (statement*) ~ expr ^^ {case ss ~ e => new FunctionBody(ss, e)}
  def function:       Parser[Function]            = funcSignature ~ ("->" ~> funcBody) ^^ { case sig ~ body => new Function(sig, body, sig.T) }

  def number:         Parser[Number]              = """\d+(\.\d*)?""".r ^^ { case n => new Number(n.toDouble)}

  def expr:           Parser[Expr]                = ("(" ~> expr <~ ")") | function | number
  def statement:      Parser[Statement]           = expr <~ ";" ^^ {case e => new Statement(e)}
}
