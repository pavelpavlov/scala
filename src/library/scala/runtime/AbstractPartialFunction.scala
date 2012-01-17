/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2011, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

abstract class AbstractPartialFunction[-T1, +R] extends AbstractFunction1[T1, R] with PartialFunction[T1, R]

/**
 * This class is used as base class for partial function literals with
 * certainly exhaustive pattern matchers.
 */
abstract class ExhaustivePFLiteral[-T1, +R] extends AbstractPartialFunction[T1, R] with PartialFunction.Total[T1, R]

/**
 * This class is used as base class for partial function literals with
 * non-exhaustive pattern matchers.
 */
abstract class PFLiteral[-T1, +R] extends AbstractPartialFunction[T1, R] with PartialFunction.WithDefault[T1, R]
