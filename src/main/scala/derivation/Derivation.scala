package derivation

import scala.compiletime.*
import scala.deriving.Mirror

trait Derivation[TypeClass[_]]:
  def sumTypeClass[T](sum: SumType[TypeClass, T]): TypeClass[T]

  def productTypeClass[T](product: ProductType[TypeClass, T]): TypeClass[T]

  inline given derived[T](using mirror: Mirror.Of[T]): TypeClass[T] =
    inline mirror match
      case sum:     Mirror.SumOf[T]     => deriveSum(sum)
      case product: Mirror.ProductOf[T] => deriveProduct(product)

  inline def deriveSum[T](sum: Mirror.SumOf[T]): TypeClass[T] =
    val subClasses = summonSubClasses[T, sum.MirroredElemTypes, sum.MirroredElemLabels](0)
    val sumType = SumType[TypeClass, T](sum, subClasses)
    sumTypeClass(sumType)

  inline def deriveProduct[T](product: Mirror.ProductOf[T]): TypeClass[T] =
    val fields = summonFields[T, product.MirroredElemTypes, product.MirroredElemLabels](0)
    val productType = ProductType[TypeClass, T](fields)
    productTypeClass(productType)

  inline def summonSubClasses[T, Subs <: Tuple, Names <: Tuple](index: Int): List[SumType.SubClass[TypeClass, T]] =
    inline erasedValue[(Subs, Names)] match
      case _: (EmptyTuple, EmptyTuple) =>
        Nil
      case _: ((s *: st), (n *: nt)) =>
        val name = constValue[n].asInstanceOf[String]
        SumType.SubClass[TypeClass, T, s](name, index, summonInline[TypeClass[s]])
          :: summonSubClasses[T, st, nt](index + 1)

  inline def summonFields[T, Fields <: Tuple, Names <: Tuple](index: Int): List[ProductType.Field[TypeClass, T]] =
    inline erasedValue[(Fields, Names)] match
      case _: (EmptyTuple, EmptyTuple) =>
        Nil
      case _: ((f *: ft), (n *: nt)) =>
        val name = constValue[n].asInstanceOf[String]
        ProductType.Field[TypeClass, T, f](name, index, summonInline[TypeClass[f]])
          :: summonFields[T, ft, nt](index + 1)
end Derivation