import playwright.lang._
import org.scalatest._
import Inside._
import playwright.playwright

class ParserSpec extends FunSpec {
  describe("Functions") {
    describe("when fully typed with simple return value") {
      it("Should work with 1 parameter") {
        assert(playwright.parseProgram("(a:Foo):Bar -> 1").exists(_ == Program(List(Statement(
          Function(
            FunctionSignature(List(Param("a", Some("Foo"))), Some("Bar")),
            FunctionBody(List[Statement](), Number(1))
          )
        )))))
      }
      it("Should work with 2 parameters") {
        assert(playwright.parseProgram("(a:Foo, b:Bar):Baz -> 1").exists(_ == Program(List(Statement(
          Function(
            FunctionSignature(List(Param("a", Some("Foo")), Param("b", Some("Bar"))), Some("Baz")),
            FunctionBody(List[Statement](), Number(1))
          )
        )))))
      }
    }
    describe("when partially typed") {
      describe("with missing param types") {
        it("Should work with 1 untyped parameter") {
          assert(playwright.parseProgram("(a):Foo-> 1").exists(_ == Program(List(Statement(
            Function(
              FunctionSignature(List(Param("a", None)), Some("Foo")),
              FunctionBody(List[Statement](), Number(1))
            )
          )))))
        }
        it("Should work with 2 untyped parameters") {
          assert(playwright.parseProgram("(a, b):Foo-> 1").exists(_ == Program(List(Statement(
            Function(
              FunctionSignature(List(Param("a", None), Param("b", None)), Some("Foo")),
              FunctionBody(List[Statement](), Number(1))
            )
          )))))
        }
        it("Should work mixed param types") {
          assert(playwright.parseProgram("(a, b:Foo):Bar -> 1").exists(_ == Program(List(Statement(
            Function(
              FunctionSignature(List(Param("a", None), Param("b", Some("Foo"))), Some("Bar")),
              FunctionBody(List[Statement](), Number(1))
            )
          )))))
        }
      }
      describe("with missing return type") {
        describe("with no paramers") {
          it("Should work with empty bracket notation") {
            assert(playwright.parseProgram("()-> 1").exists(_ == Program(List(Statement(
              Function(
                FunctionSignature(List[Param](), None),
                FunctionBody(List[Statement](), Number(1))
              )
            )))))
          }
          it("Should work with no bracket notation") {
            assert(playwright.parseProgram("-> 1").exists(_ == Program(List(Statement(
              Function(
                FunctionSignature(List[Param](), None),
                FunctionBody(List[Statement](), Number(1))
              )
            )))))
          }
        }
        it("Should work with 1 typed parameter") {
          assert(playwright.parseProgram("(a:Foo)-> 1").exists(_ == Program(List(Statement(
            Function(
              FunctionSignature(List(Param("a", Some("Foo"))), None),
              FunctionBody(List[Statement](), Number(1))
            )
          )))))
        }
        it("Should work with 2 untyped parameters") {
          assert(playwright.parseProgram("(a, b)-> 1").exists(_ == Program(List(Statement(
            Function(
              FunctionSignature(List(Param("a", None), Param("b", None)), None),
              FunctionBody(List[Statement](), Number(1))
            )
          )))))
        }
        it("Should work mixed param types") {
          assert(playwright.parseProgram("(a, b:Foo) -> 1").exists(_ == Program(List(Statement(
            Function(
              FunctionSignature(List(Param("a", None), Param("b", Some("Foo"))), None),
              FunctionBody(List[Statement](), Number(1))
            )
          )))))
        }
      }
      describe("when totally untyped") {
        it("Should work with 1 parameter") {
          assert(playwright.parseProgram("(a)-> 1").exists(_ == Program(List(Statement(
            Function(
              FunctionSignature(List(Param("a", None)), None),
              FunctionBody(List[Statement](), Number(1))
            )
          )))))
        }
        it("Should work with 5 parameters") {
          assert(playwright.parseProgram("(a, b, c, d, e)-> 1").exists(_ == Program(List(Statement(
            Function(
              FunctionSignature(List(Param("a", None), Param("b", None), Param("c", None), Param("d", None), Param("e", None)), None),
              FunctionBody(List[Statement](), Number(1))
            )
          )))))
        }
      }
    }
  }
}
