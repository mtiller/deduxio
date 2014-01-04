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
    println(sets);
    assert(sets.size==24);
    assert(sets.contains((0,1)))
    assert(!sets.contains((0,4)))
  }
  test("Test GreaterThan") {
    val c = GreaterThan("A", "B");
    val sets = c.evaluate(Samples.B1);
    println(sets);
    assert(sets.size==27);
    assert(!sets.contains((0,1)))
    assert(sets.contains((1,0)))
  }
  test("Test LessThan") {
    val c = LessThan("A", "B");
    val sets = c.evaluate(Samples.B1);
    println(sets);
    assert(sets.size==27);
    assert(sets.contains((0,1)))
    assert(!sets.contains((1,0)))
  }
  test("Test SameColor") {
    val c = SameColor("A", "B");
    val sets = c.evaluate(Samples.B1);
    println(sets);
    assert(sets.size==18);
    assert(sets.contains((0,1)))
    assert(!sets.contains((0,3)))
  }
  test("Test SameColumn") {
    val c = SameColumn("A", "B");
    val sets = c.evaluate(Samples.B1);
    println(sets);
    assert(sets.size==18);
    assert(!sets.contains((0,1)))
    assert(sets.contains((0,3)))
  }
  test("Test SamePath") {
    val c = SamePath("A", "B");
    val sets = c.evaluate(Samples.B1);
    println(sets);
    assert(sets.size==18);
    assert(sets.contains((0,1)))
    assert(!sets.contains((0,3)))
  }
  test("Test SameRow") {
    val c = SameRow("A", "B");
    val sets = c.evaluate(Samples.B1);
    println(sets);
    assert(sets.size==18);
    assert(sets.contains((0,1)))
    assert(!sets.contains((0,3)))
  }
}
