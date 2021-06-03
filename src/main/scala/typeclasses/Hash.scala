package typeclasses

import derivation.{Derivation, ProductType, SumType}

trait Hash[T]:
  def hash(t: T): Int

object Hash extends Derivation[Hash]:
  def apply[T](using ev: Hash[T]): Hash[T] = ev

  override def sumTypeClass[T](sum: SumType[Hash, T]): Hash[T] =
    new Hash[T]:
      def hash(t: T): Int =
        val sub = sum.subClass(t)
        sub.typeClass.hash(sub.cast(t))

  override def productTypeClass[T](product: ProductType[Hash, T]): Hash[T] =
    new Hash[T]:
      def hash(t: T): Int =
        product.fields
          .map { field =>
            field.typeClass.hash(field.extractParam(t))
          }
          .foldLeft(0)(31 * _ + _)

  given Hash[Int] = identity(_)
  given Hash[String] = _.hashCode
end Hash