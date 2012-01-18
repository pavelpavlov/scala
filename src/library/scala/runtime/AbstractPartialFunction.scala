/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2011, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

abstract class AbstractPartialFunction2[-T1, +R] extends AbstractFunction1[T1, R] with PartialFunction[T1, R]

// TEMPORARY PATCH {{{
abstract class AbstractPartialFunction[-T1, +R] extends AbstractFunction1[T1, R] with PartialFunction[T1, R] {
  protected def missingCase(x: T1): R = throw new MatchError(x)

  def isDefinedAt(x: T1): scala.Boolean = _isDefinedAt(x)
  def _isDefinedAt(x: T1): scala.Boolean
}

// }}} TEMPORARY PATCH

/**
 * This class is used as base class for partial function literals with
 * non-exhaustive pattern matchers.
 */
abstract class PFLiteral[-T1, +R] extends AbstractPartialFunction2[T1, R] with PartialFunction.Optimized[T1, R]

/**
 * This class is used as base class for partial function literals with
 * certainly exhaustive pattern matchers.
 */
abstract class XPFLiteral[-T1, +R] extends AbstractPartialFunction2[T1, R] with PartialFunction.Total[T1, R]
