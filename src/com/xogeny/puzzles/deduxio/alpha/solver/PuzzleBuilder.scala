package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.cons._

/**
 * Created by mtiller on 1/4/14.
 */
case class PuzzleBuilder(prob: Problem, sol: Map[String,Int], scorer: Scorer) {
  val baseConstraints = {
    val pgens = List(IsNumber, IsColor);
    val sgens = List(SamePath);
    val cons = (pgens ::: sgens) flatMap { _.valid(prob, sol) }
    cons.sortBy { scorer.score(_) }
  }
  private def trim(given: List[Constraint], left: List[Constraint], solver: Solver): List[Constraint] = left match {
    case Nil => given
    case x :: y => solver.impose(given ::: y).solve() match {
      case sols if sols.size>1 => trim(x :: given, y, solver)
      case _ => trim(given, y, solver)
    }
  }
  def craft() = trim(Nil, baseConstraints, SetSolver.forProblem(prob))
}
