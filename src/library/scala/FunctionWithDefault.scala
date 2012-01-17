/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** A function with default of type `FunctionWithDefault[A, B]` is a
 *  unary partial function where the domain does not necessarily include all
 *  values of type `A`. The function `isDefinedAt` allows to
 *  test dynamically if a value is in the domain of the function.
 *
 *  `FunctionWithDefault` reformulates all operations of its supertrait PartialFunction
 *  in terms of `isDefinedAt` and `applyOrElse`.
 *  This allows more effective implementations in many cases:
 *  - optimized `orElse` method supports chained `orElse` in linear time,
 *    and with no slow-down if the `orElse` part is not needed.
 *  - optimized `lift` method helps to avoid double evaluation of pattern matchers & guards
 *    of partial function literals.
 *   
 *  This trait is used as a basis for implementation of all partial function literals
 *  with non-exhaustive matchers. It is recommended to use `FunctionWithDefault`
 *  instead of `PartialFunction` as a base trait for user-defined partial functions.
 *
 *  @author  Martin Odersky
 *  @author  Pavel Pavlov
 *  @since   2.10
 */

trait FunctionWithDefault[-A, +B] extends PartialFunction[A, B] { self =>
  import PartialFunction.{FallBackToken, fallback, liftFallBack}

  def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1

  final def apply(x: A): B = applyOrElse(x, PartialFunction.empty)

  override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] = 
    new PartialFunction.OrElse[A1, B1] (this, that)

  override def andThen[C](k: B => C) : FunctionWithDefault[A, C] = new runtime.AbstractFunctionWithDefault[A, C] {
    def isDefinedAt(x: A): Boolean = self.isDefinedAt(x)
    def applyOrElse[A1 <: A, C1 >: C](x: A1, default: A1 => C1): C1 =
      self.applyOrElse(x, fallback) match {
        case FallBackToken => default(x)
        case z => k(z.asInstanceOf[B])
      }
  }

  override def lift: A => Option[B] = (x: A) => liftFallBack[B](applyOrElse(x, fallback))
}
