package karme.graphs

import karme.util.MapUtil

object Graphs {
  trait VertexLike extends Ordered[VertexLike] {
    def id: String

    override def compare(that: VertexLike): Int = this.id.compare(that.id)
  }

  trait EdgeLike[Vertex] {
    def v1: Vertex
    def v2: Vertex
  }

  case class UnlabeledEdge[Vertex](v1: Vertex, v2: Vertex)
    extends EdgeLike[Vertex]
  case class LabeledEdge[Vertex, Label](v1: Vertex, v2: Vertex, label: Label)
    extends EdgeLike[Vertex]

  trait GraphLike[Vertex <: VertexLike, Edge <: EdgeLike[Vertex],
    G <: GraphLike[Vertex, Edge, G]] {
    def V: Set[Vertex]
    def E: Set[Edge]

    def undirectedNeighbors(v: Vertex): Set[Vertex] = {
      var vs = Set[Vertex]()
      for (e <- E) {
        if (e.v1 == v) {
          vs += e.v2
        } else if (e.v2 == v) {
          vs += e.v1
        }
      }
      vs
    }

    def addVertex(v: Vertex): G
    def addEdge(v1: Vertex, v2: Vertex): G

  }

  trait DigraphLike[Vertex <: VertexLike, Edge <: EdgeLike[Vertex],
    G <: GraphLike[Vertex, Edge, G]] {
    this: G =>

    def edgeDirections: Map[Edge, Set[EdgeDirection]]

    // target cache
    private lazy val vertexToTargets: Map[Vertex, Set[Vertex]] = {
      def computeTargets(v: Vertex): Set[Vertex] = {
        var vs = Set[Vertex]()
        for (e <- E) {
          for (d <- edgeDirections(e)) {
            if (source(e, d) == v) {
              vs += target(e, d)
            }
          }
        }
        vs
      }

      (V map (v => v -> computeTargets(v))).toMap
    }

    private lazy val vertexToSources: Map[Vertex, Set[Vertex]] = {
      MapUtil.reverse(vertexToTargets)
    }

    def targets(v: Vertex): Set[Vertex] = {
      vertexToTargets(v)
    }

    def sources(v: Vertex): Set[Vertex] = {
      vertexToSources(v)
    }

    def enumeratePathsWithLen(len: Int): Seq[IndexedSeq[Vertex]] = {
      require(len >= 0)

      if (len > 0) {
        val pathsToExtend = enumeratePathsWithLen(len - 1)

        pathsToExtend flatMap { vs =>
          targets(vs.last) collect {
            case target if !vs.contains(target) => vs :+ target
          }
        }
      } else {
        this.V.toSeq map (v => IndexedSeq(v))
      }
    }

    def shortestPaths(src: Vertex): Set[Seq[Vertex]] = {
      var prev = Map[Vertex, Vertex]()
      var dist = Map[Vertex, Int](src -> 0)
      var toProcess = Set(src)

      def reconstructPath(v: Vertex): Seq[Vertex] = {
        require(dist.isDefinedAt(v))
        prev.get(v) match {
          case Some(p) => reconstructPath(p) ++ Seq(v)
          case None => Seq(v)
        }
      }

      while (toProcess.nonEmpty) {
        val u = toProcess.minBy(dist(_))
        toProcess -= u

        for (v <- this.targets(u)) {
          // constant distance
          val altDist = dist(u) + 1
          dist.get(v) match {
            case Some(d) if d <= altDist => {
              // neighbor is already reachable
            }
            case _ => {
              dist += (v -> altDist)
              prev += (v -> u)
              toProcess += v
            }
          }
        }
      }

      dist.keySet.map(reconstructPath)
    }

    def source(edge: Edge, direction: EdgeDirection): Vertex = direction match {
      case Forward => edge.v1
      case Backward => edge.v2
    }

    def target(edge: Edge, direction: EdgeDirection): Vertex = direction match {
      case Forward => edge.v2
      case Backward => edge.v1
    }
  }

  case class UnlabeledGraph[Vertex <: VertexLike](
    V: Set[Vertex] = Set.empty,
    E: Set[UnlabeledEdge[Vertex]] = Set.empty[UnlabeledEdge[Vertex]]
  ) extends GraphLike[Vertex, UnlabeledEdge[Vertex], UnlabeledGraph[Vertex]] {

    def addVertex(v: Vertex) = new UnlabeledGraph(V + v, E)

    def addEdge(v1: Vertex, v2: Vertex) = {
      new UnlabeledGraph(V + v1 + v2, E + lexicographicEdge(v1, v2))
    }

    def removeVertex(v: Vertex) = {
      val edgesWithoutVertexToRemove = E filter {
        case UnlabeledEdge(v1, v2) => v != v1 && v != v2
      }
      new UnlabeledGraph(V - v, edgesWithoutVertexToRemove)
    }

    def removeEdge(v1: Vertex, v2: Vertex) = {
      new UnlabeledGraph(V, E - lexicographicEdge(v1, v2))
    }

  }

  case class UnlabeledDiGraph[Vertex <: VertexLike](
    V: Set[Vertex] = Set.empty[Vertex],
    E: Set[UnlabeledEdge[Vertex]] = Set.empty[UnlabeledEdge[Vertex]],
    edgeDirections: Map[UnlabeledEdge[Vertex], Set[EdgeDirection]] =
      Map.empty[UnlabeledEdge[Vertex], Set[EdgeDirection]]
  ) extends GraphLike[Vertex, UnlabeledEdge[Vertex], UnlabeledDiGraph[Vertex]]
    with DigraphLike[Vertex, UnlabeledEdge[Vertex], UnlabeledDiGraph[Vertex]] {

    def addVertex(v: Vertex) = {
      new UnlabeledDiGraph[Vertex](V + v, E, edgeDirections)
    }

    def addEdge(v1: Vertex, v2: Vertex) = {
      val (newEdge, dir) = if (v1 < v2) {
        (UnlabeledEdge(v1, v2), Forward)
      } else {
        (UnlabeledEdge(v2, v1), Backward)
      }
      new UnlabeledDiGraph(
        V + v1 + v2,
        E + newEdge,
        MapUtil.addBinding(edgeDirections, newEdge, dir))
    }

    def removeVertex(v: Vertex) = {
      val edgesToRemove = E filter {
        case UnlabeledEdge(v1, v2) => v == v1 || v == v2
      }
      val filteredEdgeDirs = edgeDirections filter {
        case (e, ds) => !edgesToRemove.contains(e)
      }
      new UnlabeledDiGraph(V - v, E -- edgesToRemove, filteredEdgeDirs)
    }

    def removeEdge(e: UnlabeledEdge[Vertex]) = {
      new UnlabeledDiGraph(V, E - e, edgeDirections - e)
    }

  }

  sealed trait EdgeDirection
  case object Forward extends EdgeDirection
  case object Backward extends EdgeDirection

  def lexicographicEdge[Vertex <: VertexLike](
    v1: Vertex, v2: Vertex
  ): UnlabeledEdge[Vertex] = {
    if (v1.id < v2.id) {
      UnlabeledEdge(v1, v2)
    } else {
      UnlabeledEdge(v2, v1)
    }
  }
}
