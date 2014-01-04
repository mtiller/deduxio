package com.xogeny.puzzles.deduxio.tests.alpha.solver

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.solver._
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/4/14.
 */
class TestProblemGenerator extends FunSuite {
  test("Board generation") {
    val board = ProblemGenerator.randomBoard(0, (3, 3), 3, List(Red, Blue, Green))
    println(board);
  }
}
