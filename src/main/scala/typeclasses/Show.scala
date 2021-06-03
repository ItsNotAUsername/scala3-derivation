package typeclasses

import derivation.{Derivation, ProductType, SumType}

trait Show[T]:
  def show(t: T): String

object Show extends Derivation[Show]:
  def apply[T](using ev: Show[T]): Show[T] = ev

  override def sumTypeClass[T](sum: SumType[Show, T]): Show[T] =
    new Show[T]:
      def show(t: T): String =
        val sub = sum.subClass(t)
        sub.typeClass.show(sub.cast(t))

  override def productTypeClass[T](product: ProductType[Show, T]): Show[T] =
    new Show[T]:
      def show(t: T): String =
        product.fields
          .map { field =>
            field.name + ": " + field.typeClass.show(field.extractParam(t))
          }
          .mkString("{", ", ", "}")
        
  given Show[Int] = _.toString
  given Show[String] = identity(_)
end Show
