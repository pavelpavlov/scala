/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2011, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

abstract class AbstractPartialFunction2[-T1, +R] extends F1[T1, R] with PartialFunction[T1, R]

/**
 * This class is used as base class for partial function literals with
 * non-exhaustive pattern matchers.
 */
abstract class PF[-T1, +R] extends AbstractPartialFunction2[T1, R] with PartialFunction.Optimized[T1, R] with Serializable

/**
 * This class is used as base class for partial function literals with
 * certainly exhaustive pattern matchers.
 */
abstract class XPF[-T1, +R] extends AbstractPartialFunction2[T1, R] with PartialFunction.Total[T1, R] with Serializable
