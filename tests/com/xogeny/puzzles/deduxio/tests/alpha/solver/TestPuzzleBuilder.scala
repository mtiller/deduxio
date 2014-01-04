package com.xogeny.puzzles.deduxio.tests.alpha.solver

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.solver._

/**
 * Created by mtiller on 1/4/14.
 */
class TestPuzzleBuilder extends FunSuite {
  test("Puzzle builder algorithm for 3x3 problem") {
    val (prob, sol) = ProblemGenerator.generate(0, (3, 3), 3, List(Red, Green, Blue))
    val builder = PuzzleBuilder(0, prob, sol, PvS)
    val cons = builder.craft();
    assert(cons.length>0)
  }
  test("Puzzle builder algorithm for 4x4 problem") {
    val (prob, sol) = ProblemGenerator.generate(0, (4, 4), 4, List(Red, Green, Blue, Yellow))
    val builder = PuzzleBuilder(0, prob, sol, PvS)
    val cons = builder.craft();
    assert(cons.length>0)
  }
  //test("Puzzle builder algorithm for 5x5 problem") {
  //  val (prob, sol) = ProblemGenerator.generate(0, (5, 5), 5, List(Red, Green, Blue, Yellow, Purple))
  //  val builder = PuzzleBuilder(0, prob, sol, PvS)
  //  val cons = builder.craft();
  //  assert(cons.length>0)
  //}
  //test("Puzzle builder algorithm for 6x6 problem") {
  //  val (prob, sol) = ProblemGenerator.generate(0, (6, 6), 6, List(Red, Green, Blue, Yellow, Purple, Cyan))
  //  val builder = PuzzleBuilder(0, prob, sol, PvS)
  //  val cons = builder.craft();
  //  assert(cons.length>0)
  //}
}
