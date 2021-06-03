package typeclasses

import derivation.*

trait Eq[T]:
  def eqv(x: T, y: T): Boolean
  extension (x: T)
    def === (y: T): Boolean = eqv(x, y)
    def =/= (y: T): Boolean = !eqv(x, y)

object Eq extends Derivation[Eq]:
  def apply[T](using ev: Eq[T]): Eq[T] = ev

  override def sumTypeClass[T](sum: SumType[Eq, T]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        val xSub = sum.subClass(x)
        val ySub = sum.subClass(y)
        (xSub.index == ySub.index)
          && xSub.typeClass.eqv(xSub.cast(x), xSub.cast(y))

  override def productTypeClass[T](product: ProductType[Eq, T]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        product.fields.forall { field =>
          field.typeClass.eqv(
            field.extractParam(x),
            field.extractParam(y)
          )
        }

  given Eq[Int] = _ == _
  given Eq[String] = _ == _
end Eq
