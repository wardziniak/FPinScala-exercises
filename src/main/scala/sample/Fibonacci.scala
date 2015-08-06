package sample

import scala.annotation.tailrec

/**
 * Created by bartek on 8/6/15.
 */
object Fibonacci {

  def fib(n: Int): Int = {
    @tailrec
    def fibHelper(currentIndex: Int, oneStepBackValue: Int, twoStepBackValue: Int): Int = currentIndex match {
      case `n` => oneStepBackValue + twoStepBackValue
      case 1 => fibHelper(2, 1, 0)
      case m: Int => fibHelper(m+1, oneStepBackValue+twoStepBackValue, oneStepBackValue)
    }
    fibHelper(1, 1, 0)
  }

}
