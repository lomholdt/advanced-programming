// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
//
// Simple scenario based tests for term evaluators of Wadler
// (actually there is only two tests, that are taken directly from the paper)
// (more could be taken from later sections)
package adpro.monads

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class  TermSpec extends FlatSpec {

  // two test cases from Section 2.1 [Wadler]:

  val answer = Div (
                  Div (Con (1972), Con (2)),
                  Con (23)
               )

  val error = Div ( Con (1), Con (0) )

  // Section 2.1

  behavior of "Basic eval (answer)"
  it should "be 42 [Wadler]" in { BasicEvaluator.eval (answer) shouldBe 42 }

  behavior of "Basic eval (error)"
  it should "throw an exception" in
  { intercept[java.lang.ArithmeticException] { BasicEvaluator.eval (error) } }

// Uncomment sections below as you proceed

//   // Section 2.2
//
//   behavior of "Exception eval (answer)"
//   it should "be Return(42) [Wadler]" in {
//     assert (ExceptionEvaluator.eval (answer) == ExceptionEvaluator.Return(42))
//   }
//
//   behavior of "Exception eval (error)"
//   it should "throw an exception" in {
//     (ExceptionEvaluator.eval (error)) shouldBe a [ExceptionEvaluator.Raise]
//   }
//
//   // Section 2.3 [Wadler] Variation two: State
//
//   behavior of "State eval (answer)"
//   it should "give (42,2)" in {
//    assert (StateEvaluator.eval (answer).step (0) == (42,2))
//   }
//
//   // Section 2.4 [Wadler] Variation three: Output
//
//   val result = "eval(Con(1972)) <= 1972\n" +
//                "eval(Con(2)) <= 2\n" +
//                "eval(Div(Con(1972),Con(2))) <= 986\n" +
//                "eval(Con(23)) <= 23\n" +
//                "eval(Div(Div(Con(1972),Con(2)),Con(23))) <= 42\n"
//
//   behavior of "Output eval (answer)"
//   it should "give good 'result' and string output" in {
//     val r = OutputEvaluator.eval(answer)
//     r.a shouldBe 42
//     r.o shouldBe result
//   }
//
//   // Section 2.6 [Wadler] Variation zero, revisited: The basic evaluator
//
//   behavior of "Basic monadic eval (answer)"
//   it should "be 42 [Wadler]" in { BasicEvaluatorWithMonads.eval (answer).a shouldBe 42 }
//
//   behavior of "Basic monadic eval (error)"
//   it should "throw an exception" in
//   { intercept[java.lang.ArithmeticException] { BasicEvaluatorWithMonads.eval (error) } }
//
//   // Section 2.7 [Wadler] Variation one, revisited: Exceptions
//
//   behavior of "Monadic exception eval (answer)"
//   it should "be Return(42) [Wadler]" in {
//     assert (ExceptionEvaluatorWithMonads.eval (answer) == ExceptionEvaluatorWithMonads.Return(42))
//   }
//
//   behavior of "Monadic exception eval (error)"
//   it should "throw an exception" in {
//     (ExceptionEvaluatorWithMonads.eval (error)) shouldBe a [ExceptionEvaluatorWithMonads.Raise]
//   }
//
//   // Section 2.8 [Wadler] Variation two, revisited: State
//
//   behavior of "Monadic state eval (answer)"
//   it should "give (42,2)" in {
//    assert (StateEvaluatorWithMonads.eval (answer).step (0) == (42,2))
//   }
//
//   // Section 2.9 [Wadler] Output evaluator
//
//   behavior of "Monadic output eval (answer)"
//   it should "give good result and string output" in {
//     val r = OutputEvaluatorWithMonads.eval(answer)
//     r.a shouldBe 42
//     r.o shouldBe result
//   }
//
}
