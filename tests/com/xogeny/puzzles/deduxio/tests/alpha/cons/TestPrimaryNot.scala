package com.xogeny.puzzles.deduxio.tests.alpha.cons

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.tests.alpha.Samples
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/4/14.
 */
class TestPrimaryNot extends FunSuite {
  test("Test PrimaryNot(IsNumber) constraint class") {
    val c = IsNumber("A", 1);
    val n = PrimaryNot(c);
    val sets = n.evaluate(Samples.B1);
    assert(sets==Set(1, 2, 4, 5, 7, 8))
  }
  test("Test PrimaryNot(IsColor(Red)) constraint class") {
    val c = IsColor("A", Red);
    val n = PrimaryNot(c);
    val sets = n.evaluate(Samples.B1);
    assert(sets==Set(3, 4, 5, 6, 7, 8))
  }
  test("Test PrimaryNot(IsColumn(1)) constraint class") {
    val c = IsColumn("A", 1);
    val n = PrimaryNot(c);
    val sets = n.evaluate(Samples.B1);
    assert(sets==Set(1, 2, 4, 5, 7, 8))
  }
  test("Test PrimaryNot(IsRow(1)) constraint class") {
    val c = IsRow("A", 1);
    val n = PrimaryNot(c);
    val sets = n.evaluate(Samples.B1);
    assert(sets==Set(3, 4, 5, 6, 7, 8))
  }
  test("Test PrimaryNot(IsOnPath(Blue)) constraint class") {
    val c = IsOnPath("A", Blue);
    val n = PrimaryNot(c);
    val sets = n.evaluate(Samples.B1);
    assert(sets==Set(3, 4, 5, 6, 7, 8))
  }
}
