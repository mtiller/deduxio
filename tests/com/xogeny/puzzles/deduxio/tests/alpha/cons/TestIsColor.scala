package com.xogeny.puzzles.deduxio.tests.alpha.cons

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.tests.alpha._
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/4/14.
 */
class TestIsColor extends FunSuite {
  test("Test IsColor(Red) constraint class") {
    val c = IsColor("A", Red);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(0, 1, 2))
  }
  test("Test IsColor(Blue) constraint class") {
    val c = IsColor("A", Blue);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(3, 4, 5))
  }
  test("Test IsColor(Green) constraint class") {
    val c = IsColor("A", Green);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(6, 7, 8))
  }

  test("IsColor generator") {
    val prob = Problem(Samples.B1, Set("A"));
    val sol = Map("A" -> 0);
    val cons = IsColor.valid(prob, sol);
    cons.toList forall { c: Constraint =>
      c.consistent(Samples.B1, sol);
    }
  }
}
