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

  def IdentityMonad: Monad[Identity] = {
    object IdentityMonad extends Monad[Identity] {
      def flatMap[A, B](a: Identity[A], f: A => Identity[B]): Identity[B] = {
        f(a.a)
      }

      def unital[A](a: A): Identity[A] = {
        Identity(a)
      }
    }
    IdentityMonad
  }
}

object MonadicFunctions {
  // 6. Replace error("todo") with an implementation
  def sequence[M[_], A](as: List[M[A]], m: Monad[M]): M[List[A]] =
    error("todo")
 
  // 7. Replace error("todo") with an implementation
  def fmap[M[_], A, B](a: M[A], f: A => B, m: Monad[M]): M[B] =
    m.flatMap(a, (x: A) => m.unital(f(x)))
 
  // 8. Replace error("todo") with an implementation
  def flatten[M[_], A](a: M[M[A]], m: Monad[M]): M[A] =
    error("todo")
 
  // 9. Replace error("todo") with an implementation
  def apply[M[_], A, B](f: M[A => B], a: M[A], m: Monad[M]): M[B] =
    error("todo")
 
  // 10. Replace error("todo") with an implementation
  def filterM[M[_], A](f: A => M[Boolean], as: List[A]
    , m: Monad[M]): M[List[A]] =
    error("todo")
 
  // 11. Replace error("todo") with an implementation
  def replicateM[M[_], A](n: Int, a: M[A], m: Monad[M]): M[List[A]] =
    error("todo: flatMap n times to produce a list")
 
  // 12. Replace error("todo") with an implementation
  def lift2[M[_], A, B, C](f: (A, B) => C, a: M[A], b: M[B]
    , m: Monad[M]): M[C] =
    error("todo")
 
  // lift3, lift4, etc. Interesting question: Can we have liftN?
}