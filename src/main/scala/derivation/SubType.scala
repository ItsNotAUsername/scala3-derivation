package derivation

trait SubClass[TypeClass[_], Type](val name: String, val index: Int):
  type SubType
  def typeClass: TypeClass[SubType]
  def cast(t: Type): SubType

object SubClass:
  def apply[TC[_], T, ST](name: String, ind: Int, tc: => TC[ST]): SubClass[TC, T] =
    new SubClass[TC, T](name, ind):
      type SubType = ST
      lazy val typeClass: TC[ST] = tc
      def cast(t: T): ST = t.asInstanceOf[ST]
