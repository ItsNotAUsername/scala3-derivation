package derivation

import scala.deriving.Mirror

trait SumType[TypeClass[_], Type](val name: String, val subClasses: List[SubClass[TypeClass, Type]]):
  def subClass(t: Type): SubClass[TypeClass, Type]

object SumType:
  def apply[TC[_], T](mirror: Mirror.SumOf[T], name: String, subs: List[SubClass[TC, T]]): SumType[TC, T] =
    new SumType[TC, T](name, subs):
      def subClass(t: T): SubClass[TC, T] =
        val ord = mirror.ordinal(t)
        subClasses.find(_.index == ord).get
