package com.xogeny.puzzles.deduxio.tests.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.tests.alpha.Samples
import org.scalatest.FunSuite

/**
 * Created by mtiller on 1/4/14.
 */
class TestIsOnPath extends FunSuite {
  test("Test IsOnPath(Blue) constraint class") {
    val c = IsOnPath("A", Blue);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(0, 1, 2))
  }
  test("Test IsOnPath(Green) constraint class") {
    val c = IsOnPath("A", Green);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(3, 4, 5))
  }
  test("Test IsOnPath(Red) constraint class") {
    val c = IsOnPath("A", Red);
    val sets = c.evaluate(Samples.B1);
    assert(sets==Set(6, 7, 8))
  }

  test("Test IsOnPath generator") {
    val cons = IsOnPath.generate(Samples.B1, "A", Samples.B1.spaces(6));
    assert(cons==List(IsOnPath("A", Red)))
  }
}
