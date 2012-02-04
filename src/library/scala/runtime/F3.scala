package scala.runtime

abstract class F3[-T1, -T2, -T3, +R] extends Function3[T1, T2, T3, R] with Serializable
