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
  import PartialFunction._

  /** Checks if a value is contained in the function's domain.
   *
   *  @param  x   the value to test
   *  @return `'''true'''`, iff `x` is in the domain of this function, `'''false'''` otherwise.
   */
  def isDefinedAt(x: A): Boolean

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
    new OrElse[A1, B1] (this, that)
  //TODO: why not overload it with orElse(that: F1): F1?

  /**  Composes this partial function with a transformation function that
   *   gets applied to results of this partial function.
   *   @param  k  the transformation function
   *   @tparam C  the result type of the transformation function.
   *   @return a partial function with the same domain as this partial function, which maps
   *           arguments `x` to `k(this(x))`.
   */
  override def andThen[C](k: B => C) : PartialFunction[A, C] = new APF[A, C] {
    def isDefinedAt(x: A): Boolean = self isDefinedAt x
    def apply(x: A): C = k(self(x))
  }

  /** Turns this partial function into an plain function returning an `Option` result.
   *  @see     Function.unlift
   *  @return  a function that takes an argument `x` to `Some(this(x))` if `this`
   *           is defined for `x`, and to `None` otherwise.
   */
  def lift: A => Option[B] = new Lifted(this)

  /**
   *  TODO: comment
   *  @since   2.10
   */
  def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
    if (this isDefinedAt x) apply(x) else default(x)

  /**
   *  TODO: comment
   *  @since   2.10
   */
  def run[U](x: A)(action: B => U): Boolean =
    applyOrElse(x, fallbackToken) match {
      case FallbackToken => false
      case z => action(z.asInstanceOf[B]); true
    }

  /**
   *  TODO: comment
   *  @since   2.10
   */
  def runWith[U](action: B => U): A => Boolean = { x => run(x)(action) }
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

  private[scala] type APF[-A, +B] = runtime.AbstractPartialFunction2[A, B]

  /** `PartialFunction.Optimized` reformulates all operations of its supertrait `PartialFunction`
   *  in terms of `isDefinedAt` and `applyOrElse`.
   *  This allows more effective implementations in many cases:
   *  - optimized `orElse` method supports chained `orElse` in linear time,
   *    and with no slow-down if the `orElse` part is not needed.
   *  - optimized `lift` method helps to avoid double evaluation of pattern matchers & guards
   *    of partial function literals.
   *   
   *  This trait is used as a basis for implementation of all partial function literals
   *  with non-exhaustive matchers.
   *
   *  Use of `PartialFunction.Optimized` instead of `PartialFunction` as a base trait for
   *  user-defined partial functions may result in better performance
   *  and more predictable behavior w.r.t. side effects.
   *
   *  @author  Pavel Pavlov
   *  @since   2.10
   */
  trait Optimized[-A, +B] extends PartialFunction[A, B] { self =>
    // This is ugly, but I could not find a way to make `applyOrElse` abstract here
    override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = ???

    final def apply(x: A): B = applyOrElse(x, PartialFunction.empty)

    override final def andThen[C](k: B => C) : PartialFunction[A, C] = 
      new APF[A, C] with Optimized[A, C] {
        def isDefinedAt(x: A): Boolean = self.isDefinedAt(x)
        override def applyOrElse[A1 <: A, C1 >: C](x: A1, default: A1 => C1): C1 =
          self.applyOrElse(x, fallbackToken) match {
            case FallbackToken => default(x)
            case z => k(z.asInstanceOf[B])
          }
      }
  }

  /** `Total` is partial function which `isDefinedAt` method always returns `true`.
   *  This trait is used as a basis for implementation of all partial function literals
   *  with exhaustive matchers.
   *
   *  @author  Pavel Pavlov
   *  @since   2.10
   */
  trait Total[-A, +B] extends PartialFunction[A, B] {
    final def isDefinedAt(x: A): Boolean = true
    override final def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = apply(x)
    override final def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = this
    //TODO: check generated code for PF literal here
    override final def andThen[C](k: B => C): PartialFunction[A, C] = { case x => k(apply(x)) }
  }

  /** Composite function produced by `PartialFunction#orElse` method
   */
  private final class OrElse[-A, +B] (
        final val f1: PartialFunction[A, B],
        final val f2: PartialFunction[A, B] )
      extends APF[A, B] {

    def isDefinedAt(x: A) = f1.isDefinedAt(x) || f2.isDefinedAt(x)

    def apply(x: A): B = f1.applyOrElse(x, f2)

    override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
      f1.applyOrElse(x, fallbackToken) match {
        case FallbackToken => f2.applyOrElse(x, default)
        case z => z.asInstanceOf[B]
      }

    override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) =
      new OrElse[A1, B1] (f1, f2 orElse that)

    override def andThen[C](k: B => C) =
      new OrElse[A, C] (f1 andThen k, f2 andThen k)
  }

  private[scala] case object FallbackToken
  private[scala] final val fallbackToken: PartialFunction[Any, Any] = { case _ => FallbackToken }
  //TODO: check generated code for PF literal here

  private[scala] final class Lifted[-A, +B] (val pf: PartialFunction[A, B])
      extends runtime.AbstractFunction1[A, Option[B]] {

    def apply(x: A): Option[B] = pf.applyOrElse(x, fallbackToken) match {
      case FallbackToken => None
      case z => Some(z.asInstanceOf[B])
    }
  }

  private final class Unlifted[A, B] (f: A => Option[B]) 
      extends APF[A, B] with Optimized[A, B] {

    def isDefinedAt(x: A): Boolean = f(x).isDefined
    override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = 
      f(x) getOrElse default(x)
    override def lift = f
  }

  private[scala] def unlifted[A, B](f: A => Option[B]): PartialFunction[A, B] = f match {
    case lf: Lifted[A, B] => lf.pf
    case ff => new Unlifted(ff)
  }

  /** Converts ordinary function to partial one
   *  @since   2.10
   */
  //TODO: check generated code for PF literal here
  def apply[A, B](f: A => B): PartialFunction[A, B] = { case x => f(x) }

  private[this] final val false_f: Any => Boolean = { _ => false}

  private[this] final val empty_pf: PartialFunction[Any, Nothing] = new APF[Any, Nothing] {
    def isDefinedAt(x: Any) = false
    def apply(x: Any) = throw new MatchError(x)
    override def orElse[A1, B1](that: PartialFunction[A1, B1]) = that
    override def andThen[C](k: Nothing => C) = this
    override val lift = (x: Any) => None
    override def run[U](x: Any)(action: Nothing => U) = false
    override def runWith[U](action: Nothing => U) = false_f
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
  def cond[T](x: T)(pf: PartialFunction[T, Boolean]): Boolean = pf.applyOrElse(x, false_f)

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
