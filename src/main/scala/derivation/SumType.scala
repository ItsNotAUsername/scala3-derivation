package derivation

import scala.deriving.Mirror

trait SumType[TypeClass[_], Type](val subClasses: List[SumType.SubClass[TypeClass, Type]]):
  def subClass(t: Type): SumType.SubClass[TypeClass, Type]

object SumType:
  def apply[TC[_], T](mirror: Mirror.SumOf[T], scs: List[SubClass[TC, T]]): SumType[TC, T] =
    new SumType[TC, T](scs):
      def subClass(t: T): SubClass[TC, T] =
        val ord = mirror.ordinal(t)
        subClasses.find(_.index == ord).get

  trait SubClass[TypeClass[_], Type](val name: String, val index: Int):
    type SubType
    def typeClass: TypeClass[SubType]
    def cast(t: Type): SubType

  object SubClass:
    def apply[TC[_], T, ST](name: String, ind: Int, tc: TC[ST]): SubClass[TC, T] =
      new SubClass[TC, T](name, ind):
        type SubType = ST
        def typeClass: TC[ST] = tc
        def cast(t: T): ST = t.asInstanceOf[ST]
  end SubClass
end SumType