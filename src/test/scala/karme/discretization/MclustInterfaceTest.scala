package karme.discretization

import org.scalatest.FunSuite

class MclustInterfaceTest extends FunSuite {

  test("test mclust on univariate data") {
    val data = List(2.41435, 3.050206, 3.192141, 2.969304, 2.90586, 3.001347, 2.699389, 3.275667, 3.33832, 3.156547, 2.938237, 3.186084, 2.807481, 2.535838, 2.994005, 2.747989, 3.273129, 2.723173, 3.044804, 3.044939, 10.00471, 10.3779, 10.60983, 10.31682, 9.847698, 10.2584, 10.1496, 9.674284, 10.47761, 10.0704, 9.87172, 9.968198, 9.284558, 10.1423, 9.883015, 9.805018, 9.5168, 10.06937, 9.68625, 9.759706, 10.21335, 9.579638, 9.258165, 9.546922, 10.44254, 9.783833, 9.468686, 10.13447, 9.616338, 10.13026)

    val res = MclustInterface.mclust(data, 1, 2)

    // there should be two components optimally
    assertResult(2)(res.g)

    assert(res.classification.size == data.size)

    // all uncertainty values should be between 0 and 1
    assert(res.uncertainty.size == data.size)
    assert(res.uncertainty.forall(x => x >= 0 && x <= 1))
  }

}
