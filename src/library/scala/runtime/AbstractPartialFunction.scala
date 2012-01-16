/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2011, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

abstract class AbstractPartialFunction[-T1, +R] extends AbstractFunction1[T1, R] with PartialFunction[T1, R]

/** This class provides a default implementation of partial functions
 *  that is used for all partial function literals.
 *  It contains an optimized `orElse` method which supports
 *  chained `orElse` in linear time, and with no slow-down
 *  if the `orElse` part is not needed.
 */
abstract class ComposablePartialFunction[-T1, +R]
    extends AbstractPartialFunction[T1, R] { self =>

  def apply(x: T1): R = applyOrElse(x, PartialFunction.empty)

  def applyOrElse[A1 <: T1, B1 >: R](x: T1, fallBack: PartialFunction[A1, B1]): B1

  override def orElse[A1 <: T1, B1 >: R](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] =
    new CompositePartialFunction[A1, B1] (self, that)
}

private[runtime] final class CompositePartialFunction[-T1, +R] (
      final val f1: ComposablePartialFunction[T1, R],
      final val f2: PartialFunction[T1, R] )
    extends AbstractPartialFunction[T1, R] {

  def isDefinedAt(x: T1) = f1.isDefinedAt(x) || f2.isDefinedAt(x)

  def apply(x: T1) = f1.applyOrElse(x, f2)

  override def orElse[A1 <: T1, B1 >: R](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] =
    new CompositePartialFunction[A1, B1] (f1, f2 orElse that)
}

