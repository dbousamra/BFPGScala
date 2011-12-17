trait Monad[M[_]] {
  def flatMap[A, B](a: M[A], f: A => M[B]): M[B]
  def unital[A](a: A): M[A]
}

case class Inter[A](f: Int => A)

case class Identity[A](a: A)

object Monad {

  //def ListMonad: Monad[List] = error("todo")
  def ListMonad: Monad[List] = {
    object ListMonad extends Monad[List] {
      def flatMap[A, B](a: List[A], f: A => List[B]): List[B] = {
        a flatMap f
      }
      def unital[A](a: A): List[A] = {
        List(a)
      }
    }
    ListMonad
  }

  // 3. Replace error("todo") with an implementation
  def OptionMonad: Monad[Option] = {
    object OptionMonad extends Monad[Option] {
      def flatMap[A, B](a: Option[A], f: A => Option[B]): Option[B] = {
        a match {
          case Some(value) => f(value)
          case None => None
        }
      }
      def unital[A](a: A): Option[A] = {
        Some(a)
      }
    }
    OptionMonad
  }

  // 4. Replace error("todo") with an implementation
  def InterMonad: Monad[Inter] = {
    object InterMonad extends Monad[Inter] {
      def flatMap[A, B](a: Inter[A], f: A => Inter[B]): Inter[B] = {
        val g = a match {
          case Inter(g) => g
        }
        val fn = (n: Int) => f(g(n)) match {
          case Inter(fn) => fn(n)
        }
        Inter(fn)
      }
      def unital[A](a: A): Inter[A] = {
        Inter(_ => a)
      }
    }
    InterMonad
  }

  // 5. Replace error("todo") with an implementation
  def IdentityMonad: Monad[Identity] = error("todo")
}
