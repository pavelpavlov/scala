/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** A partial function of type `PartialFunction[A, B]` is a unary function
 *  where the domain does not necessarily include all values of type `A`.
 *  The function `isDefinedAt` allows to test dynamically if a value is in
 *  the domain of the function.
 *
 *  Even if `isDefinedAt` returns true for an `a: A`, calling `apply(a)` may
 *  still throw an exception, so the following code is legal:
 *
 *  {{{
 *  val f: PartialFunction[Int, Any] = { case _ => 1/0 }
 *  }}}
 *
 *  The main distinction between `PartialFunction` and [[scala.Function1]] is
 *  that the user of a `PartialFunction` may choose to do something different
 *  with input that is declared to be outside its domain. For example:
 *
 *  {{{
 *  val sample = 1 to 10
 *  val isEven: PartialFunction[Int, String] = { 
 *    case x if x % 2 == 0 => x+" is even" 
 *  }
 *
 *  // the method collect can use isDefinedAt to select which members to collect
 *  val evenNumbers = sample collect isEven
 *
 *  val isOdd: PartialFunction[Int, String] = { 
 *    case x if x % 2 == 1 => x+" is odd" 
 *  }
 *
 *  // the method orElse allows chaining another partial function to handle 
 *  // input outside the declared domain
 *  val numbers = sample map (isEven orElse isOdd)
 *  }}}
 *
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait PartialFunction[-A, +B] extends (A => B) { self =>

  /** Checks if a value is contained in the function's domain.
   *
   *  @param  x   the value to test
   *  @return `'''true'''`, iff `x` is in the domain of this function, `'''false'''` otherwise.
   */
  def isDefinedAt(x: A): Boolean

  //TODO: consider declaring and providing base implementation of `applyOrElse` here

  /** Composes this partial function with a fallback partial function which
   *  gets applied where this partial function is not defined.
   *
   *  @param   that    the fallback function
   *  @tparam  A1      the argument type of the fallback function
   *  @tparam  B1      the result type of the fallback function
   *  @return  a partial function which has as domain the union of the domains
   *           of this partial function and `that`. The resulting partial function
   *           takes `x` to `this(x)` where `this` is defined, and to `that(x)` where it is not.
   */
  def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] =
    new runtime.AbstractPartialFunction[A1, B1] with PartialFunction.WithDefault[A1, B1] {
      def isDefinedAt(x: A1) = self.isDefinedAt(x) || that.isDefinedAt(x)
      def applyOrElse[A2 <: A1, B2 >: B1](x: A2, default: A2 => B2): B2 = {
        if (self.isDefinedAt(x)) self(x)
        else if (that.isDefinedAt(x)) that(x)
        else default(x)
      }
    }
    /*TODO: replace `orElse` by this code when PF literals will be ready: {
      case x if self.isDefinedAt(x) => self(x)
      case x if that.isDefinedAt(x) => that(x)
    }*/

  //TODO: do we need to override here Function1.compose as well?
  /**  Composes this partial function with a transformation function that
   *   gets applied to results of this partial function.
   *   @param  k  the transformation function
   *   @tparam C  the result type of the transformation function.
   *   @return a partial function with the same domain as this partial function, which maps
   *           arguments `x` to `k(this(x))`.
   */
  override def andThen[C](k: B => C) : PartialFunction[A, C] = new runtime.AbstractPartialFunction[A, C] {
    def isDefinedAt(x: A): Boolean = self.isDefinedAt(x)
    def apply(x: A): C = k(self.apply(x)) //TODO: try to implement better
  }

  /** Turns this partial function into an plain function returning an `Option` result.
   *  @see     Function.unlift
   *  @return  a function that takes an argument `x` to `Some(this(x))` if `this`
   *           is defined for `x`, and to `None` otherwise.
   */
  def lift: A => Option[B] = {
    val pf = this orElse PartialFunction.fallback
    (x: A) => PartialFunction.liftFallBack[B](pf(x))
  }
}

/** A few handy operations which leverage the extra bit of information
 *  available in partial functions.  Examples:
 *  {{{
 *  import PartialFunction._
 *
 *  def strangeConditional(other: Any): Boolean = cond(other) {
 *    case x: String if x == "abc" || x == "def"  => true
 *    case x: Int => true
 *  }
 *  def onlyInt(v: Any): Option[Int] = condOpt(v) { case x: Int => x }
 *  }}}
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
object PartialFunction {

  /** `WithDefault` reformulates all operations of its supertrait `PartialFunction`
   *  in terms of `isDefinedAt` and `applyOrElse`.
   *  This allows more effective implementations in many cases:
   *  - optimized `orElse` method supports chained `orElse` in linear time,
   *    and with no slow-down if the `orElse` part is not needed.
   *  - optimized `lift` method helps to avoid double evaluation of pattern matchers & guards
   *    of partial function literals.
   *   
   *  This trait is used as a basis for implementation of all partial function literals
   *  with non-exhaustive matchers. It is recommended to use `WithDefault`
   *  instead of `PartialFunction` as a base trait for user-defined partial functions.
   *
   *  @author  Pavel Pavlov
   *  @since   2.10
   */
  trait WithDefault[-A, +B] extends PartialFunction[A, B] { self =>

    def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1

    final def apply(x: A): B = applyOrElse(x, PartialFunction.empty)

    override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] = 
      new OrElse[A1, B1] (this, that)

    override def andThen[C](k: B => C) : WithDefault[A, C] =
      new runtime.AbstractPartialFunction[A, C] with WithDefault[A, C] {
        def isDefinedAt(x: A): Boolean = self.isDefinedAt(x)
        def applyOrElse[A1 <: A, C1 >: C](x: A1, default: A1 => C1): C1 =
          self.applyOrElse(x, fallback) match {
            case FallBackToken => default(x)
            case z => k(z.asInstanceOf[B])
          }
      }

    override def lift: A => Option[B] = (x: A) => liftFallBack[B](applyOrElse(x, fallback))
  }

  /** `Total` is partial function which `isDefinedAt` method always returns `true`.
   *  This trait is used as a basis for implementation of all partial function literals
   *  with exhaustive matchers. It is recommended to use `WithDefault`
   *
   *  @author  Pavel Pavlov
   *  @since   2.10
   */
  trait Total[-A, +B] extends PartialFunction[A, B] { self =>

    final def isDefinedAt(x: A) = true

    override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) = self

    override def andThen[C](k: B => C) = new runtime.AbstractPartialFunction[A, C] with Total[A, C] {
      def apply(x: A): C = k(self(x))
    }
    /* TODO when PF literals will ready replace `andThen` by:
    override def andThen[C](k: B => C) = { case x => k(self(x)) }
    */

    override def lift = (x: A) => Some(self(x))
  }

  /** Composite function produced by `WithDefault#orElse` method
   */
  private final class OrElse[-A, +B] (
        final val f1: WithDefault[A, B],
        final val f2: PartialFunction[A, B] )
      extends runtime.AbstractPartialFunction[A, B] {

    def isDefinedAt(x: A) = f1.isDefinedAt(x) || f2.isDefinedAt(x)

    def apply(x: A) = f1.applyOrElse(x, f2)

    override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) =
      new OrElse[A1, B1] (f1, f2 orElse that)

    override def andThen[C](k: B => C) =
      new OrElse[A, C] (f1 andThen k, f2 andThen k)

    override def lift = {
      val pf2 = f2 orElse fallback
      (x: A) => liftFallBack[B](f1.applyOrElse(x, pf2))
    }
  }


  //TODO: check implementation
  private[scala] def unlifted[A, B](f: A => Option[B]): PartialFunction[A, B] =
    new runtime.AbstractPartialFunction[A, B] with WithDefault[A, B] {
      def isDefinedAt(x: A): Boolean = f(x).isDefined
      def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = f(x) getOrElse default(x)
      override def lift = f
    }

  private[scala] case object FallBackToken

  private[scala] final val fallback: PartialFunction[Any, Any] =
    new runtime.AbstractPartialFunction[Any, Any] with Total[Any, Any] {
      def apply(x: Any): Any = FallBackToken
    }
  /* TODO when PF literals will ready replace `fallback` by:
  private[scala] final val fallback: PartialFunction[Any, Any] = { case _ => FallBackToken }
  */

  private[scala] def liftFallBack[A](x: Any): Option[A] = x match {
    case FallBackToken => None
    case z => Some(z.asInstanceOf[A])
  }

//  /** Converts ordinary function to partial one */
//  def apply[A, B](f: A => B): PartialFunction[A, B] = new runtime.AbstractPartialFunction[A, B] with Total[A, B] {
//    def apply(x: A): B = f(x)
//  }
//  /* TODO when PF literals will ready replace `apply` by:
//  def apply[A, B](f: A => B): PartialFunction[A, B] = { case x => f(x) }
//  */

  private[this] final val empty_pf: PartialFunction[Any, Nothing] = new runtime.AbstractPartialFunction[Any, Nothing] {
    def isDefinedAt(x: Any) = false
    def apply(x: Any) = throw new MatchError(x)
    override def orElse[A1, B1](that: PartialFunction[A1, B1]) = that
    override def andThen[C](k: Nothing => C) = this
    override val lift = (x: Any) => None
  }
  def empty[A, B] : PartialFunction[A, B] = empty_pf

  /** Creates a Boolean test based on a value and a partial function.
   *  It behaves like a 'match' statement with an implied 'case _ => false'
   *  following the supplied cases.
   *
   *  @param  x   the value to test
   *  @param  pf  the partial function
   *  @return true, iff `x` is in the domain of `pf` and `pf(x) == true`.
   */
  def cond[T](x: T)(pf: PartialFunction[T, Boolean]): Boolean =
    (pf isDefinedAt x) && pf(x)

  /** Transforms a PartialFunction[T, U] `pf` into Function1[T, Option[U]] `f`
   *  whose result is `Some(x)` if the argument is in `pf`'s domain and `None`
   *  otherwise, and applies it to the value `x`.  In effect, it is a
   *  `'''match'''` statement which wraps all case results in `Some(_)` and
   *  adds `'''case''' _ => None` to the end.
   *
   *  @param  x     the value to test
   *  @param  pf    the PartialFunction[T, U]
   *  @return `Some(pf(x))` if `pf isDefinedAt x`, `None` otherwise.
   */
  def condOpt[T,U](x: T)(pf: PartialFunction[T, U]): Option[U] = pf.lift(x)
}
