/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** A function with default of type `FunctionWithDefault[A, B]` is a
 *  unary function where the domain does not necessarily include all values of type
 *  `A`. The function `isDefinedAt` allows to
 *  test dynamically if a value is in the domain of the function.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait FunctionWithDefault[-A, +B] extends (A => B) {

  def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1

  def apply(x: A): B = applyOrElse(x, throw new MatchError(x))

  /** Composes this function with default with a fallback function with default which gets applied where this function with default
   *  is not defined.
   *
   *  @param   that    the fallback function
   *  @tparam  A1      the argument type of the fallback function
   *  @tparam  B1      the result type of the fallback function
   *  @return  a function with default which has as domain the union of the domains
   *           of this function with default and `that`. The resulting function with default
   *           takes `x` to `this(x)` where `this` is defined, and to `that(x)` where it is not.
   */
  def orElse[A1 <: A, B1 >: B](that: FunctionWithDefault[A1, B1]) : FunctionWithDefault[A1, B1] = 
    new FunctionWithDefault[A1, B1] {
    def applyOrElse[A2 <: A1, B2 >: B1](x: A2, default: A2 => B2): B2 = 
      FunctionWithDefault.this.applyOrElse(x, that)
  }

  /**  Composes this function with default with a transformation function that gets applied
   *   to results of this function with default.
   *   @param  k  the transformation function
   *   @tparam C  the result type of the transformation function.
   *   @return a function with default with the same domain as this function with default, which maps
   *           arguments `x` to `k(this(x))`.
   */
  override def andThen[C](k: B => C) : FunctionWithDefault[A, C] = new FunctionWithDefault[A, C] {
    def applyOrElse[A1 <: A, C1 >: C](x: A1, default: A1 => C1): C1 =
      try {
        k(FunctionWithDefault.this.applyOrElse(x, throw FunctionWithDefault.DoDefault))
      } catch {
        case ex: FunctionWithDefault.DefaultException => default(x)
      }
  }

  /** Turns this function with default into an plain function returning an `Option` result.
   *  @see     Function1#unlift
   *  @return  a function that takes an argument `x` to `Some(this(x))` if `this`
   *           is defined for `x`, and to `None` otherwise.
   */
  def lift: A => Option[B] = new (A => Option[B]) {
    def apply(x: A): Option[B] = 
      try {
        Some(FunctionWithDefault.this.applyOrElse(x, throw FunctionWithDefault.DoDefault)) 
      } catch {
        case ex: FunctionWithDefault.DefaultException => None
      }
/* needs adaptation in Function1
    override def unlift[R1](implicit ev: Option[B] <:< Option[R1]): FunctionWithDefault[A, R1] =
      FunctionWithDefault.this.asInstanceOf[FunctionWithDefault[A, R1]]
      */
  }
}

/** A few handy operations which leverage the extra bit of information
 *  available in functions with default.  Examples:
 * 
 * <pre>
 *  import FunctionWithDefault._
 *
 *  def strangeConditional(other: Any): Boolean = cond(other) {
 *    case x: String if x == "abc" || x == "def"  => true
 *    case x: Int => true
 *  }
 *  def onlyInt(v: Any): Option[Int] = condOpt(v) { case x: Int => x }
 * </pre>
 * 
 *  @author  Paul Phillips
 *  @since   2.8
 */
object FunctionWithDefault
{
  /** Creates a Boolean test based on a value and a function with default.
   *  It behaves like a 'match' statement with an implied 'case _ => false'
   *  following the supplied cases.
   *
   *  @param  x   the value to test
   *  @param  pf  the function with default
   *  @return true, iff `x` is in the domain of `pf` and `pf(x) == true`.
   */
  def cond[T](x: T)(pf: FunctionWithDefault[T, Boolean]): Boolean =
    pf.applyOrElse[T, Boolean](x, _ => false)
  
  /** Transforms a FunctionWithDefault[T, U] `pf' into Function1[T, Option[U]] `f'
   *  whose result is Some(x) if the argument is in pf's domain and None otherwise,
   *  and applies it to the value `x'.  In effect, it is a 'match' statement
   *  which wraps all case results in Some(_) and adds 'case _ => None' to the end.
   *
   *  @param  x     the value to test
   *  @param  pf    the FunctionWithDefault[T, U]
   *  @return `Some(pf(x))` if `pf isDefinedAt x`, `None` otherwise.
   */
  def condOpt[T,U](x: T)(pf: FunctionWithDefault[T, U]): Option[U] =
    pf.lift(x)

  private class DefaultException extends Exception
  private val DoDefault = new DefaultException
}
