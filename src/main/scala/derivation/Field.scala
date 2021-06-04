package derivation

trait Field[TypeClass[_], Type](val name: String, val index: Int):
  type Param
  def typeClass: TypeClass[Param]
  def extractParam(t: Type): Param

object Field:
  def apply[TC[_], T, P](name: String, ind: Int, tc: => TC[P]): Field[TC, T] =
    new Field[TC, T](name, ind):
      type Param = P
      lazy val typeClass: TC[P] = tc
      def extractParam(t: T): P = 
        t.asInstanceOf[Product].productElement(index).asInstanceOf[P]
