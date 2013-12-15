package com.xogeny.puzzles.simd

import JaCoP.core.{Store, IntVar}

/**
 * Created by mtiller on 12/14/13.
 */

/**
 * Used to build statement of Simd problems
 * @param r
 * @param c
 */
class Simd(r: Int, c: Int) {

}

class ProblemGenerator(board: Board, root: String, other: List[String], diff: Int=1) {
  def solve: List[SimdConstraint] = other match {
    case Nil => solveRootOnly
    case y => {
      val cons = y flatMap { n => new ProblemGenerator(board, n, Nil, diff).solve }
      cons
    }
  }
  def solveRootOnly: List[SimdConstraint] = {
    Nil
  }
}

object TestSimd {
  def main(args: Array[String]): Unit = {
    simpleSolve();
  }

  def simpleSolve() = {
    val board = Board(List(Space(Black, 1, Set(Red), 0, 0), Space(Black, 2, Set(Green), 0, 0)))
    val pos = SimdConstraint.allValid(board, Map("alpha" -> 0, "beta" -> 1))
    println("Possible constraints: "+pos);
    val prob = Problem(board, List("alpha", "beta"))
    prob.impose(pos);
    println(prob.store);
    prob.solve();
  }
}