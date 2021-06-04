package derivation

trait ProductType[TypeClass[_], Type](val name: String, val fields: List[Field[TypeClass, Type]])

object ProductType:
  def apply[TC[_], T](name: String, fields: List[Field[TC, T]]): ProductType[TC, T] =
    new ProductType[TC, T](name, fields) {}
