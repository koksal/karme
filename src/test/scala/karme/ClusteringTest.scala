package karme

import org.scalatest.FunSuite

class ClusteringTest extends FunSuite {
  test("member to cluster map") {
    val clustering = Clustering(Map(
      "c1" -> Set("a", "b"),
      "c2" -> Set("c", "d")
    ))

    assertResult("c1")(clustering.memberToCluster("a"))
    assertResult("c1")(clustering.memberToCluster("b"))
    assertResult("c2")(clustering.memberToCluster("c"))
    assertResult("c2")(clustering.memberToCluster("d"))
  }
}
