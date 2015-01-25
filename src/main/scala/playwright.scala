package playwright

import lang._

object playwright {
  def toJS(t: Transpilable):String = t match {
    case Param(name, t)               => name
    case Statement(op)                => s"${toJS(op)};"
    case Return(expr)                 => s"return ${toJS(new Statement(expr))}"
    case FunctionSignature(params, t) => params.map(toJS).mkString(",")
    case FunctionBody(stats, retval)  => (stats.map(toJS) :+ toJS(new Return(retval))).mkString(" ")
    case Function(sig, body)          => s"function (${toJS(sig)}) { ${toJS(body)} }"
    case Number(v)                    => v.toString
    case Program(ss)                  => ss.map(toJS).mkString(" ")
  }

  def parse(raw: String): String = {
    toJS(PlaywrightParser.parseAll(PlaywrightParser.program, raw).get)
  }

  def main (args: Array[String]) {
    while (true) {
      val l = readLine("$ ")
      if (l == null) {return}
      println(parse(l))
    }
  }
}
