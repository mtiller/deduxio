package com.xogeny.puzzles.deduxio.tests.alpha.cons

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.tests.alpha.Samples

/**
 * Created by mtiller on 1/3/14.
 */
class TestIsNumber extends FunSuite {
  test("Test IsNumber constraint class") {
    val c = IsNumber("A", 1);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(0, 3, 6))
  }
  test("Test IsNumber constraint generator for B1") {
    val prob = Problem(Samples.B1, Set("A", "B"));
    val cons = IsNumber.valid(prob, Map("A" -> 0, "B" -> 1));
    assert(cons==Set(IsNumber("B",2), IsNumber("A", 1)))
  }
}
