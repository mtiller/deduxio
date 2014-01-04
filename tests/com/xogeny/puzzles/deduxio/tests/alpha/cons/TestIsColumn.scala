package com.xogeny.puzzles.deduxio.tests.alpha.cons

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.tests.alpha.Samples

/**
 * Created by mtiller on 1/4/14.
 */
class TestIsColumn extends FunSuite {
  test("Test IsColumn(1) constraint class") {
    val c = IsColumn("A", 1);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(0, 3, 6))
  }
  test("Test IsColumn(2) constraint class") {
    val c = IsColumn("A", 2);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(1, 4, 7))
  }
  test("Test IsColumn(3) constraint class") {
    val c = IsColumn("A", 3);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(2, 5, 8))
  }
}
