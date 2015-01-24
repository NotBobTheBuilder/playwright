import scala.util.parsing.combinator.RegexParsers

trait Transpilable {
  def toJS: String
}

class Type
object Number extends Type
object Null extends Type
object Undefined extends Type
object String extends Type
object Function extends Type

object Type {
  def withName(n: String) = {
    n match {
      case "Number" => Number
      case "Null" => Null
      case "Undefined" => Undefined
      case "String" => String
      case "Function" => Function
    }
  }
}

class Param (
  _name: String,
  _T: Type
) extends Transpilable {
  var name = _name
  var T = _T
  def toJS(): String = {name}
}

abstract class Expr extends Transpilable

class Statement (
  op: Expr
) extends Transpilable {
  def toJS = { s"${op.toJS};"}
}

class Return (
  _expr: Expr
) extends Transpilable {
  val expr = _expr
  def toJS = {s"return ${new Statement(expr).toJS}"}
}

class FunctionSignature (
  _params: List[Param],
  _T: Type
) extends Transpilable {
  val T = _T
  val params = _params
  def toJS(): String = { params.map(_.toJS()).mkString(",") }
}

class FunctionBody (
  _stats: List[Statement],
  _retval: Expr
) extends Transpilable {
  val stats = _stats
  val retval = _retval
  def toJS(): String = {
    (stats.map(_.toJS) :+ new Return(_retval).toJS).mkString(" ")
  }
}

class Function (
  _sig: FunctionSignature,
  _body: FunctionBody,
  _T: Type
) extends Expr with Transpilable {
  var sig = _sig
  var body = _body
  var T = _T
  def toJS(): String = {
    s"function (${sig.toJS()}) { ${body.toJS()} }"
  }
}

class Number (
  _v: Double
) extends Expr with Transpilable {
  var v = _v
  def toJS(): String = {v.toString()}
}

object PlaywrightParser extends RegexParsers {
  def ident:          Parser[String]              = """\w+""".r
  def typeIdent:      Parser[Type]                = ident ^^ Type.withName
  def param:          Parser[Param]               = ident ~ (":" ~> typeIdent)  ^^ { case p ~ t => new Param(p, t)}

  def paramList:      Parser[List[Param]]         = param ~ (("," ~> param)*)   ^^ { case p ~ ps => p :: ps }

  def funcSignature:  Parser[FunctionSignature]   = ("(" ~> (paramList?) <~ ")") ~ (":" ~> typeIdent) ^^ {
    case Some(ps) ~ t => new FunctionSignature(ps, t)
    case None ~ t => new FunctionSignature(List(), t)
  }
  def funcBody:       Parser[FunctionBody]        = (statement*) ~ expr ^^ {case ss ~ e => new FunctionBody(ss, e)}
  def function:       Parser[Function]            = funcSignature ~ ("->" ~> funcBody) ^^ { case sig ~ body => new Function(sig, body, sig.T) }

  def number:         Parser[Number]              = """\d+(\.\d*)?""".r ^^ { case n => new Number(n.toDouble)}

  def expr:           Parser[Expr]                = ("(" ~> expr <~ ")") | function | number
  def statement:      Parser[Statement]           = expr <~ ";" ^^ {case e => new Statement(e)}
}
