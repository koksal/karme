package karme.graphs

import karme.graphs.Graphs._
import org.scalatest.FunSuite

class GraphsTest extends FunSuite {
  case class TestVertex(id: String) extends VertexLike

  test("shortest paths in directed graphs") {

    val V = Set(TestVertex("1"), TestVertex("2"), TestVertex("3"),
      TestVertex("4"), TestVertex("5"))

    val E = Set[UnlabeledEdge[TestVertex]](
      UnlabeledEdge(TestVertex("1"), TestVertex("2")),
      UnlabeledEdge(TestVertex("1"), TestVertex("3")),
      UnlabeledEdge(TestVertex("2"), TestVertex("4")),
      UnlabeledEdge(TestVertex("3"), TestVertex("5")),
      UnlabeledEdge(TestVertex("4"), TestVertex("5"))
    )
    val edgeDir = E.map{ e =>
      e -> Set[EdgeDirection](Forward)
    }.toMap

    val g = UnlabeledDiGraph[TestVertex](V, E, edgeDir)

    val res = g.shortestPaths(TestVertex("1"))

    val expected = Set(
      Seq(TestVertex("1")),
      Seq(TestVertex("1"), TestVertex("2")),
      Seq(TestVertex("1"), TestVertex("3")),
      Seq(TestVertex("1"), TestVertex("2"), TestVertex("4")),
      Seq(TestVertex("1"), TestVertex("3"), TestVertex("5"))
    )

    assertResult(expected)(res)
  }
}
