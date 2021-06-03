import org.scalatest.flatspec.AnyFlatSpec
import typeclasses.*

class DerivationTest extends AnyFlatSpec:

  case class Person(name: String, age: Int) derives Eq, Hash, Show

  enum Maybe[+T] derives Eq, Hash, Show:
    case Just(t: T)
    case None

  val p1 = Person("Nikita", 21)
  val p2 = Person("Sasha", 22)

  it should "generate Eq instance" in {
    val personEq = summon[Eq[Person]]
    assert(personEq.eqv(p1, p2) == false)
    assert(p1 =/= p2)
    assert(p1 === p1)
    assert(p2 === p2)

    val maybeEq = summon[Eq[Maybe[Int]]]
    assert(maybeEq.eqv(Maybe.Just(1), Maybe.Just(1)))
    assert(Maybe.Just(1) =/= Maybe.Just(2))
    assert(Maybe.Just(1) === Maybe.Just(1))
    assert(Maybe.None === Maybe.None)
  }

  it should "generate Show instance" in {
    val personShow = summon[Show[Person]]
    assert(personShow.show(p1) == "{name: Nikita, age: 21}")
    assert(personShow.show(p2) == "{name: Sasha, age: 22}")

    val maybeShow = summon[Show[Maybe[Int]]]
    assert(maybeShow.show(Maybe.Just(1)) == "{t: 1}")
    assert(maybeShow.show(Maybe.None) == "{}")
  }

end DerivationTest
