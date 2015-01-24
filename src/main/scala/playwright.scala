object playwright {
  def toJS(t: Transpilable):String = t match {
    case Param(name, string)          => name
    case Statement(op)                => s"${toJS(op)};"
    case Return(expr)                 => s"return ${toJS(new Statement(expr))}"
    case FunctionSignature(params, t) => params.map(toJS).mkString(",")
    case FunctionBody(stats, retval)  => (stats.map(toJS) :+ toJS(new Return(retval))).mkString(" ")
    case Function(sig, body, t)       => s"function (${toJS(sig)}) { ${toJS(body)} }"
    case Number(v)                    => v.toString()
  }

  def main (args: Array[String]) {
    for (ln <- io.Source.stdin.getLines) {
      val res = PlaywrightParser.parseAll(PlaywrightParser.function, ln)
      if (res.successful) {
        println(toJS(res.get))
      } else {
        println("error!")
      }
    }
  }
}
