package sample

/**
 * Created by bartek on 8/6/15.
 */
object SampleExercises {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(position: Int): Boolean = {
      if (position == as.length) true
      else if (position == 0) loop(1)
      else if (!ordered(as(position-1), as(position)))
        false
      else loop(position+1)
    }
    loop(0)
  }

}
