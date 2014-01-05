package com.xogeny.puzzles.deduxio.tests.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.tests.alpha.Samples
import org.scalatest.FunSuite

/**
 * Created by mtiller on 1/4/14.
 */
class TestSecondaryConstraints extends FunSuite {
  test("Test AdjacentTo") {
    val c = AdjacentTo("A", "B");
    val sets = c.evaluate(Samples.B1);
    assert(sets.size==24);
    assert(sets.contains((0,1)))
    assert(!sets.contains((0,4)))
  }
  test("Test GreaterThan") {
    val c = GreaterThan("A", "B");
    val sets = c.evaluate(Samples.B1);
    assert(sets.size==27);
    assert(!sets.contains((0,1)))
    assert(sets.contains((1,0)))
  }
  test("Test LessThan") {
    val c = LessThan("A", "B");
    val sets = c.evaluate(Samples.B1);
    assert(sets.size==27);
    assert(sets.contains((0,1)))
    assert(!sets.contains((1,0)))
  }
  test("Test SameColor") {
    val c = SameColor("A", "B");
    val sets = c.evaluate(Samples.B1);
    assert(sets.size==18);
    assert(sets.contains((0,1)))
    assert(!sets.contains((0,3)))
  }
  test("Test SameColumn") {
    val c = SameColumn("A", "B");
    val sets = c.evaluate(Samples.B1);
    assert(sets.size==18);
    assert(!sets.contains((0,1)))
    assert(sets.contains((0,3)))
  }
  test("Test SamePath") {
    val c = SamePath("A", "B");
    val sets = c.evaluate(Samples.B1);
    assert(sets.size==18);
    assert(sets.contains((0,1)))
    assert(!sets.contains((0,3)))
  }
  test("Test SameRow") {
    val c = SameRow("A", "B");
    val sets = c.evaluate(Samples.B1);
    assert(sets.size==18);
    assert(sets.contains((0,1)))
    assert(!sets.contains((0,3)))
  }
  test("Test SecondaryNot") {
    val l = LessThan("A", "B");
    val g = GreaterThan("A", "B");
    val e = SameNumber("A", "B");
    val lset = l.evaluate(Samples.B1);
    val gset = g.evaluate(Samples.B1);
    val eset = e.evaluate(Samples.B1);
    val nl = SecondaryNot(l);
    val ng = SecondaryNot(g);
    val nlset = nl.evaluate(Samples.B1);
    val ngset = ng.evaluate(Samples.B1);

    assert(lset ++ eset == ngset);
    assert(gset ++ eset == nlset);
  }

  test("More GreaterThan testing") {
    val c = GreaterThan("C", "B");
    val sol = Map("A" -> 0, "B" -> 7, "C" -> 8);
    val bv = Samples.B1.spaces(7).number;
    val cv = Samples.B1.spaces(8).number;
    assert(c.consistent(Samples.B1, sol));
    assert(cv>bv);
  }
}
