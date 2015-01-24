object playwright {
  def main (args: Array[String]) {
    for (ln <- io.Source.stdin.getLines) {
      val res = PlaywrightParser.parseAll(PlaywrightParser.function, ln)
      if (res.successful) {
        println(res.get.toJS)
      } else {
        println("error!")
      }
    }
  }
}
