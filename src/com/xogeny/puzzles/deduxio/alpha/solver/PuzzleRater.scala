package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/5/14.
 */

/**
 * This class is used to "rate" puzzles.
 * @param prob A given problem
 * @param cons A given set of constraints
 * @param sol A given solution
 */
class PuzzleRater(prob: Problem, cons: List[Constraint], sol: Map[String,Int]) {
  /**
   * This method analyzes a problem and returns information about the difficulty
   * @return The number of possible spaces for each variable (using only primary constraints to deduce them)
   */
  def analyze() = {
    val solver = SetSolver.forProblem(prob).impose(cons);
    val steps = solver.vals map { p => p._1 -> p._2.size}
    steps
  }
}
