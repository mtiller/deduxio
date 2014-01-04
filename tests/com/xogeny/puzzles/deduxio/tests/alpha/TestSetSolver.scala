package com.xogeny.puzzles.deduxio.tests.alpha

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.solver.SetSolver
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.cons._

/**
 * Created by mtiller on 1/3/14.
 */
class TestSetSolver extends FunSuite {
  test("Creation from problem") {
    val ss = SetSolver.forProblem(Problem(Samples.B1, Set("A", "B")))
    assert(ss.vals.get("A").get==Set(0, 1, 2, 3, 4, 5, 6, 7, 8))
    assert(ss.vals.get("B").get==Set(0, 1, 2, 3, 4, 5, 6, 7, 8))
    assert(ss.cons==Nil)
    assert(ss.involving.get("A")==Some(Nil));
    assert(ss.involving.get("B")==Some(Nil));
  }

  test("Applying primary constraints") {
    val ss1 = SetSolver.forProblem(Problem(Samples.B1, Set("A", "B")))
    val ss2 = ss1.impose(IsNumber("A", 1))
    assert(ss2.vals.get("A").get==Set(0, 3, 6));
    assert(ss2.vals.get("B").get==Set(0, 1, 2, 3, 4, 5, 6, 7, 8))
    val ss3 = ss2.impose(IsNumber("B", 2))
    assert(ss3.vals.get("A").get==Set(0, 3, 6));
    assert(ss3.vals.get("B").get==Set(1, 4, 7))
  }

  test("Applying secondary constraints") {
    val ss1 = SetSolver.forProblem(Problem(Samples.B1, Set("A", "B")));
    val c1 = SamePath("A", "B")
    val ss2 = ss1.impose(c1);
    assert(ss2.cons==List(c1));
    assert(ss2.involving.get("A")==Some(List(c1)));
    assert(ss2.involving.get("B")==Some(List(c1)));
    assert(ss2.involving.get("C")==None);
  }

  test("Simple solver test") {
    val ss1 = SetSolver.forProblem(Problem(Samples.B1, Set("A")));
    val bsols = ss1.solve();
    assert(bsols.size==9);
    val c1 = IsNumber("A", 1);
    val ss2 = ss1.impose(c1);
    val sols = ss2.solve();
    println("Solutions: "+sols);
    assert(sols==Set(Map("A" -> 0), Map("A" -> 3), Map("A" -> 6)))
  }

  test("Test collision detection") {
    val ss1 = SetSolver.forProblem(Problem(Samples.B1, Set("A", "B")));
    val bsols = ss1.solve();
    assert(bsols.size==72);
  }
}
