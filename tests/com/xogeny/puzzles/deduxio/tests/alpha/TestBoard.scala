package com.xogeny.puzzles.deduxio.tests.alpha

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */
class TestBoard extends FunSuite {
  test("Space numbers in B1") {
    assert(Samples.B1.spaces(0).number==1);
    assert(Samples.B1.spaces(1).number==2);
    assert(Samples.B1.spaces(2).number==3);

    assert(Samples.B1.spaces(3).number==1);
    assert(Samples.B1.spaces(4).number==2);
    assert(Samples.B1.spaces(5).number==3);

    assert(Samples.B1.spaces(6).number==1);
    assert(Samples.B1.spaces(7).number==2);
    assert(Samples.B1.spaces(8).number==3);

  }
  test("Adjacent squares in B1") {
    assert(Samples.B1.adjacent(4)==Set(1,3,5,7))
  }
  test("Paths in B1") {
    println(Samples.B1.paths);
    assert(Samples.B1.paths(0)==Set(Blue))
    assert(Samples.B1.paths(Samples.B1.spaces(0))==Set(Blue))
  }
}
