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
}
