package playwright.lang

import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps

trait Transpilable

case class Param (
  name: String,
  T: Option[String]
) extends Transpilable

abstract class Expr extends Transpilable

case class Program (
  statements: List[Statement]
) extends Transpilable

case class Statement (
  op: Expr
) extends Transpilable

case class Return (
  expr: Expr
) extends Transpilable

case class FunctionSignature (
  params: List[Param],
  T: Option[String]
) extends Transpilable

case class FunctionBody (
  stats: List[Statement],
  retval: Expr
) extends Transpilable

case class Function (
  sig: FunctionSignature,
  body: FunctionBody
) extends Expr with Transpilable

case class Number (
  v: Double
) extends Expr with Transpilable

object PlaywrightParser extends RegexParsers {
  def ident:          Parser[String]              = """\w+""".r
  def typeIdent:      Parser[String]              = ident
  def param:          Parser[Param]               = ident ~ ((":" ~> typeIdent)?)  ^^ { case p ~ t => Param(p, t)}

  def paramList:      Parser[List[Param]]         = param ~ (("," ~> param)*)   ^^ { case p ~ ps => p :: ps }

  def funcSignature:  Parser[FunctionSignature]   = (("(" ~> paramList <~ ")")?) ~ ((":" ~> typeIdent)?) ^^ {
    case ps ~ t       => FunctionSignature(ps.getOrElse(List[Param]()), t)
  }
  def funcBody:       Parser[FunctionBody]        = (statement*) ~ expr ^^ {case ss ~ e => FunctionBody(ss, e)}
  def function:       Parser[Function]            = funcSignature ~ ("->" ~> funcBody) ^^ { case sig ~ body => Function(sig, body) }

  def number:         Parser[Number]              = """\d+(\.\d*)?""".r ^^ { case n => Number(n.toDouble)}

  def expr:           Parser[Expr]                = ("(" ~> expr <~ ")") | function | number
  def statement:      Parser[Statement]           = expr <~ ";" ^^ {case e => Statement(e)}
  def program:        Parser[Program]             = ((statement | expr)*) ^^ {
    case l => Program(l.map {
      case Statement(s) => Statement(s)
      case x:Expr => Statement(x)
    })
  }
}
