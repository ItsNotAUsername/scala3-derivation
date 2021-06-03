package derivation

trait ProductType[TypeClass[_], Type](val fields: List[ProductType.Field[TypeClass, Type]])

object ProductType:
  def apply[TC[_], T](fs: List[Field[TC, T]]): ProductType[TC, T] =
    new ProductType[TC, T](fs) {}

  trait Field[TypeClass[_], Type](val name: String, val index: Int):
    type Param
    def typeClass: TypeClass[Param]
    def extractParam(t: Type): Param

  object Field:
    def apply[TC[_], T, P](name: String, ind: Int, tc: TC[P]): Field[TC, T] =
      new Field[TC, T](name, ind):
        type Param = P
        def typeClass: TC[P] = tc
        def extractParam(t: T): P =
          t.asInstanceOf[Product].productElement(index).asInstanceOf[P]
  end Field
end ProductType
