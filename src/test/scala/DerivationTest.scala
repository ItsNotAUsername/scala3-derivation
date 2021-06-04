import org.scalatest.flatspec.AnyFlatSpec
import typeclasses.*

class DerivationTest extends AnyFlatSpec:

  case class Person(name: String, age: Int) derives Eq, Hash, Show

  enum Maybe[+T] derives Eq, Hash, Show:
    case Just(t: T)
    case None

  enum Tree[+T] derives Eq, Hash, Show:
    case Branch(left: Tree[T], value: T, right: Tree[T])
    case Leaf

  val p1 = Person("Nikita", 21)
  val p2 = Person("Sasha", 22)

  val t1 = Tree.Branch(Tree.Leaf, 1, Tree.Branch(Tree.Leaf, 2, Tree.Leaf))
  val t2 = Tree.Branch(Tree.Leaf, 3, Tree.Leaf)

  it should "generate Eq instance" in {
    val personEq = summon[Eq[Person]]
    assert(personEq.eqv(p1, p2) == false)
    assert(p1 =/= p2)
    assert(p1 === p1)
    assert(p2 === p2)

    val maybeEq = summon[Eq[Maybe[Int]]]
    assert(maybeEq.eqv(Maybe.Just(1), Maybe.Just(1)))
    assert(Maybe.Just(1) =/= Maybe.Just(2))
    assert(Maybe.Just(1) =/= Maybe.None)
    assert(Maybe.None === Maybe.None)

    val treeEq = summon[Eq[Tree[Int]]]
    assert(treeEq.eqv(t1, t1))
    assert(t1 =/= t2)
    assert(t1 =/= Tree.Leaf)
    assert(Tree.Leaf === Tree.Leaf)
  }

  it should "generate Show instance" in {
    val personShow = summon[Show[Person]]
    assert(personShow.show(p1) == "Person(name: Nikita, age: 21)")
    assert(personShow.show(p2) == "Person(name: Sasha, age: 22)")

    val maybeShow = summon[Show[Maybe[Int]]]
    assert(maybeShow.show(Maybe.Just(1)) == "Just(t: 1)")
    assert(maybeShow.show(Maybe.None) == "None()")

    val treeShow = summon[Show[Tree[Int]]]
    assert(treeShow.show(t2) == "Branch(left: Leaf(), value: 3, right: Leaf())")
    assert(treeShow.show(Tree.Leaf) == "Leaf()")
  }

end DerivationTest
