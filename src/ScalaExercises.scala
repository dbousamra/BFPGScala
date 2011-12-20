// You are not permitted to use these List methods:
// * length
// * map
// * filter
// * ::: (and variations such as ++)
// * flatten
// * flatMap
// * reverse (and variations i.e. reverseMap, reverse_:::)
// This also means you are not permitted to use for-comprehensions on Lists.
// You are permitted to use the functions you write yourself. For example, Exercise 2 may use Exercise 1 or Exercise 3.
// Using permitted existing methods where appropriate will attract marks for elegance.

// TOTAL marks:    /66

object Exercises {

  def main(args: Array[String]) = {
    val list = (0 until 10).toList
    val list2 = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10, 11, 12))
    println(reverse(list))

  }

  def succ(n: Int) = n + 1
  def pred(n: Int) = n - 1

  // Exercise 1
  // Relative Difficulty: 1
  // Correctness: 2.0 marks
  // Performance: 0.5 mark
  // Elegance: 0.5 marks
  // Total: 3
  def add(x: Int, y: Int): Int = x match {
    case 0 => y
    case _ => add(pred(x), succ(y))
  }
  //error("todo: Assume x and y are 0 or positive. Do not use + or - on Int. Only permitted to use succ/pred (above).")

  // Exercise 2
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def sum(x: List[Int]): Int = {
    x.foldLeft(0)((x, y) => add(x, y))

  }

  // Exercise 3
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def length[A](x: List[A]): Int = {
    x.foldLeft(0)((x, y) => x + 1)
  }

  // Exercise 4
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.0 mark
  // Elegance: 1.5 marks
  // Total: 7
  def map[A, B](x: List[A], f: A => B): List[B] = {
    x.foldRight(Nil: List[B]) { (n, z) => f(n) :: z }
  }

  // Exercise 5
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def filter[A](x: List[A], f: A => Boolean): List[A] = {
    x.foldRight(Nil: List[A]) {
      (n, z) => if (f(n) == true) n :: z else z
    }
  }

  // Exercise 6
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def append[A](x: List[A], y: List[A]): List[A] = {
    x.foldRight(y) { (n, z) => z.::(n) }
  }

  // Exercise 7
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def concat[A](x: List[List[A]]): List[A] = {
    x.foldRight(Nil: List[A]) { (n, result) => append(n, result) }
  }

  // Exercise 8
  // Relative Difficulty: 7
  // Correctness: 5.0 marks
  // Performance: 1.5 marks
  // Elegance: 1.5 mark
  // Total: 8
  def concatMap[A, B](x: List[A], f: A => List[B]): List[B] = {
    x.foldRight(Nil: List[B]) { (n, result) => append(f(n), result) }
  }

  // Exercise 9
  // Relative Difficulty: 8
  // Correctness: 3.5 marks
  // Performance: 3.0 marks
  // Elegance: 2.5 marks
  // Total: 9
  def maximum(x: List[Int]): Int = {
    x.foldLeft(0) { (z, n) => if (n > z) n else z }
  }

  // Exercise 10
  // Relative Difficulty: 10
  // Correctness: 5.0 marks
  // Performance: 2.5 marks
  // Elegance: 2.5 marks
  // Total: 10
  def reverse[A](x: List[A]): List[A] = {
    x.foldLeft(Nil:List[A]){ (result,n) => result.::(n)}
  }
}
