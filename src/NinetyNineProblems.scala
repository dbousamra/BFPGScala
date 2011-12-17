object NinetyNineProblems {

  def main(args: Array[String]): Unit = {
   //println(nth(2, List(1, 2, 3, 4, 5)))
    
    
    val stack = new DomStack
  }
  
  def last[A](l: List[A]): A = l match{
    case h :: Nil => h
    case _ :: tail => last(tail)   
  }
  
  def penultimate[A](l: List[A]): A = l.takeRight(2).head
  
  def nth[A](n: Int, l: List[A]): A = l(n)
  
  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
  }
  


}