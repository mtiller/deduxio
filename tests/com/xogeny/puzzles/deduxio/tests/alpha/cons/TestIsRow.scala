package com.xogeny.puzzles.deduxio.tests.alpha.cons

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.tests.alpha.Samples

/**
 * Created by mtiller on 1/4/14.
 */
class TestIsRow extends FunSuite {
  test("Test IsRow(1) constraint class") {
    val c = IsRow("A", 1);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(0, 1, 2))
  }
  test("Test IsRow(2) constraint class") {
    val c = IsRow("A", 2);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(3, 4, 5))
  }
  test("Test IsRow(3) constraint class") {
    val c = IsRow("A", 3);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(6, 7, 8))
  }
}