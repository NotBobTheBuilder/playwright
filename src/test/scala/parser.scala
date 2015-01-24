import playwright.lang._
import org.scalatest._
import Inside._

class ParserSpec extends FunSpec {

  describe("Fully type-annotated punction") {
    val typed = PlaywrightParser.parseAll(PlaywrightParser.function, "(a:Number):Number->1")
    assert(typed.successful)
    inside (typed.get) { case Function(sig, body, funcT) =>
      inside (sig) { case FunctionSignature(params, sigT) =>
        describe("Parameters") {
          they("should be parsed correctly") {
            assert(params == List(Param("a", TNumber)))
          }
        }
        describe("Return Types") {
          they("should be Number") {
            assert(sigT == TNumber)
          }
        }
      }
      inside (body) { case FunctionBody(statements, returnVal) =>
        describe("Statements") {
          they("should be empty") {
            assert(statements == List[Statement]())
          }
        }
        describe("Return Value") {
          it("should be 1") {
            assert(returnVal == Number(1.0))
          }
        }
      }
    }
  }

  describe("Only parameter typed function") {
    val partTyped = PlaywrightParser.parseAll(PlaywrightParser.function, "(a:Number)->1")
    assert(partTyped.successful)
    inside (partTyped.get) { case Function(sig, body, funcT) =>
      inside (sig) { case FunctionSignature(params, sigT) =>
        describe("Parameters") {
          they("should be parsed correctly") {
            assert(params == List(Param("a", TNumber)))
          }
        }
        describe("Return Types") {
          they("should be parsed") {
            assert(sigT == TInferred)
          }
        }
      }
      inside (body) { case FunctionBody(statements, returnVal) =>
        describe("Statements") {
          they("should be empty") {
            assert(statements == List[Statement]())
          }
        }
        describe("Return Value") {
          it("should be 1") {
            assert(returnVal == Number(1.0))
          }
        }
      }
    }
  }

  describe("Multi-Param typed function") {
    val partTyped = PlaywrightParser.parseAll(PlaywrightParser.function, "(a:Number,b:String,c:Null)->1")
    assert(partTyped.successful)
    assert(partTyped.get == Function(
      FunctionSignature(
        List(Param("a", TNumber), Param("b", TString), Param("c", TNull)),
        TInferred
      ),
      FunctionBody(List[Statement](), Number(1.0)),
      TInferred
    ))
  }

  describe("Expressions Bracket depth") {
    val expr = PlaywrightParser.parseAll(PlaywrightParser.expr, "(((((((1)))))))")
    assert(expr.successful)
    assert(expr.get == Number(1.0))
  }

  describe("Numbers") {
    val one = PlaywrightParser.parseAll(PlaywrightParser.number, "1")
    assert(one.successful && one.get == Number(1.0))
    val onePointOne = PlaywrightParser.parseAll(PlaywrightParser.number, "1.1")
    assert(onePointOne.successful && onePointOne.get == Number(1.1))
  }

}
