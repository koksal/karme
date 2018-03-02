package karme.util

import org.scalatest.FunSuite

class CollectionUtilTest extends FunSuite {
  test("Enumerate all subsets of a two-sized set") {
    val set = Set(1, 2)
    val expected = Set(
      Set(),
      Set(1),
      Set(2),
      Set(1, 2)
    )

    assertResult(expected)(CollectionUtil.allSubsets(set))
  }

  test("Subsets of an empty set") {
    assertResult(Set(Set()))(CollectionUtil.allSubsets(Set()))
  }

  test("Combinations") {
    val set = Set(1, 2, 3)

    assertResult(
      Set(Set(1), Set(2), Set(3))
    )(
      CollectionUtil.combinations(set, 1)
    )

    assertResult(
      Set(Set())
    )(
      CollectionUtil.combinations(set, 0)
    )

    assertResult(
      Set(Set(1, 2), Set(1, 3), Set(2, 3))
    )(
      CollectionUtil.combinations(set, 2)
    )
  }
}
