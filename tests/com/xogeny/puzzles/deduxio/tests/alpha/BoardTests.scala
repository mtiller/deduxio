package com.xogeny.puzzles.deduxio.tests.alpha

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */
class BoardTests extends FunSuite {
  test("Spaces in B1") {
    assert(Samples.B1.spaces(0).number==1);
    assert(Samples.B1.paths(Samples.B1.spaces(0))==Set(Blue))
    assert(Samples.B1.adjacent(4)==Set(1,3,5,7))
  }
}
