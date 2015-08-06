import org.scalatest.FlatSpec
import sample.Fibonacci

/**
 * Created by bartek on 8/6/15.
 */
class FibonacciTest extends FlatSpec {

  "Fibonacci 6" should "8" in {
    assert (Fibonacci.fib(6) == 8)
  }

  "Fibonacci 10" should "55" in {
    assert (Fibonacci.fib(10) == 55)
  }

}
