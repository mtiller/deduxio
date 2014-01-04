package com.xogeny.puzzles.deduxio.tests.alpha.solver

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.tests.alpha.Samples
import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.solver._

/**
 * Created by mtiller on 1/4/14.
 */
class TestPuzzleBuilder extends FunSuite {
  test("Puzzle builder algorithm") {
    val prob = Problem(Samples.B1, Set("A", "B"))
    val sol = Map("A" -> 0, "B" -> 2)
    val builder = PuzzleBuilder(prob, sol, PvS)
    println("Base constraints: "+builder.baseConstraints);
    val cons = builder.craft();
    println(cons);
    assert(cons.length>0)
  }
}
